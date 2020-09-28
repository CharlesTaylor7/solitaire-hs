module Solitaire.Core.Engine where

import Solitaire.Prelude

import Solitaire.Core.Rules
import Solitaire.Core.Config

import Solitaire.Core.Move.Class
import qualified Solitaire.Core.Move.Class as Move

import qualified Data.HashSet as Set


data GameConclusion game = GameWon [game] | GameLost


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


runGame
  :: forall rs.
  ( Solitaire rs
  )
  => Config rs
  -> IO ()
runGame config = do
  result <- runGameLoop @rs
    & unApp
    & runPQueueT
    & runHistoryT
    & flip runReaderT config
  case result of
    GameLost -> do
      putStrLn "GameLost"

    GameWon games -> do
      liftIO $ putStrLn ""
      for_ games prettyPrint
      putStrLn "GameWon"


runGameLoop
  :: forall rs. Solitaire rs
  => App (Config rs) (Game rs) (GameConclusion (Game rs))
runGameLoop = do
  game <- newGame @rs
  prettyPrint game

  queueInsert 0 $ [game]
  conclusion <- loopM (\_ -> step @rs) ()
  pure conclusion


step
  :: forall rs. (Solitaire rs, Pretty (Step (Game rs)))
  => ExceptT (GameConclusion (Game rs)) (App (Config rs) (Game rs)) ()
step = do
  -- retrieve the best priority game state
  maybeMin <- queuePopMin
  case maybeMin of
    -- out of game states to try, we lost
    Nothing ->
      throwError GameLost

    -- we have game states to play from
    Just (priority, gameHistory@(game :| _ )) -> do
      -- game states can be reached multiple times via different paths
      -- verify we haven't visted this state before
      visited <- historyHas game
      when (not visited) $ do

        -- check to see if we won
        when (gameIsWon @rs game) $
          throwError $ GameWon (toList gameHistory)

        -- record we visited this game state
        saveToHistory game
        -- checking the history hash set is much cheaper than spurious extra inserts to the priority queue
        -- so we check the visited set both at insert time & queue pop time
        steps <- nextSteps @rs game
        liftIO $ putStrLn "game:\n"
        prettyPrint game
        liftIO $ putStrLn ""
        -- insert new game states reachable from this one
        ifor_ steps $ \i step -> do
          liftIO $ putStrLn $ "step " <> show i <> "\n"
          -- print the step
          prettyPrint step
          liftIO $ putStrLn $ replicate 20 '-'
          queueInsert
            (priority + 1)
            (step ^. #game <| gameHistory)
