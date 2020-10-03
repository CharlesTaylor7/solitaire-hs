module Solitaire.Core.Engine where

import Solitaire.Prelude

import Solitaire.Core.Rules hiding (App)
import qualified Solitaire.Core.Rules as Core

import Solitaire.Core.Config
import Solitaire.Core.Utils

import Solitaire.Core.Move.Class
import qualified Solitaire.Core.Move.Class as Move

import qualified Data.HashSet as Set

import System.Timeout
import GenericUtils (constructorName)

import qualified Data.Map as Map


-- | select a random point inside the unit n-dimensional ball
randomDelta :: MonadRandom m => Int -> m [Float]
randomDelta n = do
  vars <- sequence $ replicate n $ getRandomR (0, 1)
  -- nth root
  r <- (** (1 / fromIntegral n)) <$> getRandomR (0, 1)
  let
    norm = vars & sumOf (folded . to (^ 2)) & sqrt
    scalar = r / norm

  pure $ (scalar *) <$> vars


collectStats :: forall rs. Solitaire rs => AppConfig rs -> IO (Map Text Text)
collectStats config = do
  conclusions <-
    for ([1 .. config ^. #stats . #numTrials] :: [Int]) $ \i -> do
      putStrLn $ "Run #" <> show i

      game <- runGame @rs config
        & timeout (config ^. #stats . #microSecondsTimeout)
        <&> view (non Timeout)

      putStrLn $ constructorName game <> "\n"
      pure game

  let
    numTimedOut = conclusions & lengthOf (folded . #_Timeout)
    numWon = conclusions & lengthOf (folded . #_GameWon)
    numLost = conclusions & lengthOf (folded . #_GameLost)

  pure $ mempty
    & at "numTimedOut" ?~ showText numTimedOut
    & at "numWon" ?~ showText numWon
    & at "numLost" ?~ showText numLost


showText :: Show a => a -> Text
showText = fromString . show


runGame
  :: forall rs.
  ( Solitaire rs
  )
  => AppConfig rs
  -> IO (GameConclusion (Game rs))
runGame config =
  runGameLoop @rs
    & unApp
    & flip runReaderT config
    & runPQueueT
    & runHistoryT


type App rs = Core.App (AppConfig rs) (Game rs)

data GameConclusion game = GameWon [game] | GameLost | Timeout
  deriving stock (Eq, Generic)


heuristic
  :: forall rs m. (MonadReader (AppConfig rs) m, Rules rs)
  => Game rs
  -> m Float
heuristic game =
  let
    features = heuristicFeatures @rs game
  in
    reader $ sumOf
      ( #stats
      . #heuristicWeights
      . ifolded
      . withIndex
      . to (\(i, w) -> features ^?! ix i . to (* w))
      )


allMoves
  :: forall rs.
  ( Solitaire rs
  )
  => Game rs
  -> [Step (Game rs)]
allMoves game = do
  SomeMoveType (_ :: Proxy move) <- moveTypes @rs
  (uncurry Step . (_1 %~ SomeMove)) <$> Move.steps @move game


nextSteps
  :: forall rs m.
  ( Solitaire rs
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m [Step (Game rs)]

nextSteps game = do
  history <- getHistory
  let
    unvisited :: Step (Game rs) -> Bool
    unvisited = not . flip Set.member history . view #game

  pure $ allMoves @rs game & filter unvisited

runGameLoop
  :: forall rs. Solitaire rs
  => App rs (GameConclusion (Game rs))
runGameLoop = do
  game <- Core.App $ magnify #game $ newGame @rs
  priority <- heuristic @rs game
  queueInsert priority $ GameHistory 0 [game]
  conclusion <- loopM (\_ -> step @rs) ()
  pure conclusion


step
  :: forall rs. (Solitaire rs, Pretty (Step (Game rs)))
  => ExceptT (GameConclusion (Game rs)) (App rs) ()
step = do
  -- retrieve the best priority game state
  maybeMin <- queuePopMin
  case maybeMin of
    -- out of game states to try, we lost
    Nothing ->
      throwError GameLost

    -- we have game states to play from
    Just (priority, gameHistory) -> do
      let
        game = gameHistory ^. #games . head1

      -- game states can be reached multiple times via different paths
      -- verify we haven't visted this state before
      visited <- historyHas game
      when (not visited) $ do

        -- check to see if we won
        when (gameIsWon @rs game) $
          throwError $ GameWon $ gameHistory ^.. #games . folded

        -- record we visited this game state
        saveToHistory game

        -- checking the history hash set is much cheaper than spurious extra inserts to the priority queue
        -- so we check the visited set both at insert time & queue pop time
        steps <- nextSteps @rs game

        -- insert new game states reachable from this one
        ifor_ steps $ \i step -> do
          let
            moves = gameHistory ^. #moveCount . singular #_MoveCount . to fromIntegral
          h <- heuristic @rs $ step ^. #game

          queueInsert
            -- total moves made so far + estimated remaining moves
            (moves + h)
            -- update total move count & list of game states
            (gameHistory
            & #moveCount +~ 1
            & #games %~ ((step ^. #game) <|)
            )
