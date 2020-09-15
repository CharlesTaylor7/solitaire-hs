module Solitaire.Core.Engine where

import Solitaire.Prelude

import Solitaire.Core.Rules
import Solitaire.Core.Config

import Solitaire.Core.Move
import qualified Solitaire.Core.Move as Move

import qualified Data.HashSet as Set



data GameConclusion game = GameWon [SomeMove game] | GameLost


allMoves
  :: forall rs.
  ( Solitaire rs
  )
  => Config rs
  -> [SomeMove (Game rs)]
allMoves config = do
  let
    pileCount :: NumPiles
    pileCount = numPiles config

  SomeMoveType (_ :: Proxy move) <- moveTypes @rs

  SomeMove <$> Move.moves @move @(Game rs) pileCount

nextSteps
  :: forall rs m.
  ( Solitaire rs
  , MonadReader (Config rs) m
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m [Step (Game rs)]

nextSteps game = do
  config <- ask
  history <- getHistory
  let
    paired = id &&& (\move -> runReaderT (applySomeMove move game) config)
    step = Step ^. from curried
    unvisited = not . flip Set.member history . view #game

    someMoves :: [SomeMove (Game rs)]
    someMoves = allMoves @rs config

    steps :: [Step (Game rs)]
    steps = someMoves
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited

  pure steps

runGame
  :: forall rs config game.
  ( Solitaire rs
  , Config rs ~ config
  , Game rs ~ game
  )
  => config
  -> IO ()
runGame config = do
  result <- runGameLoop @rs
    & unApp
    & runPQueueT
    & runHistoryT
    & flip runReaderT config
  case result of
    (_, GameLost) -> do
      putStrLn "GameLost"

    (game, GameWon moves) -> do
      liftIO $ putStrLn ""
      throwInIO $ void $ foldM (observeGameStep @rs) game (reverse moves)
      putStrLn "GameWon"

observeGameStep
  :: forall rs m game.
  ( Solitaire rs
  , MonadError SomeException m
  -- TODO: MonadLog
  , MonadIO m
  , game ~ Game rs
  )
  => game
  -> SomeMove game
  -> m (game)
observeGameStep game move = do
  game <- applySomeMove move game
  prettyPrint game
  liftIO $ putStrLn ""
  pure game


throwInIO :: (MonadIO m, Exception e) => ExceptT e m a -> m a
throwInIO = join . fmap rightOrThrow . runExceptT


runGameLoop
  :: forall rs. Solitaire rs
  => App (Config rs) (Game rs) (Game rs, GameConclusion (Game rs))
runGameLoop = do
  game <- newGame @rs
  prettyPrint game

  queueInsert 0 $ GameWithPlayback [] game
  conclusion <- loopM (\_ -> step @rs) ()
  pure (game, conclusion)


step
  :: forall rs. Solitaire rs
  => ExceptT (GameConclusion (Game rs)) (App (Config rs) (Game rs)) ()
step = do
  -- retrieve the best priority game state
  maybeMin <- queuePopMin
  case maybeMin of
    -- out of game states to try, we lost
    Nothing ->
      throwError GameLost

    -- we have game states to play from
    Just (priority, GameWithPlayback previousMoves game) -> do
      -- game states can be reached multiple times via different paths
      -- verify we haven't visted this state before
      visited <- historyHas game
      when (not visited) $ do

        -- check to see if we won
        when (gameIsWon @rs game) $
          throwError $ GameWon previousMoves

        -- record we visited this game state
        saveToHistory game
        -- checking the history hash set is much cheaper than spurious extra inserts to the priority queue
        -- so we check the visited set both at insert time & queue pop time
        steps <- nextSteps @rs game

        -- insert new game states reachable from this one
        for_ steps $ \step -> do
            queueInsert
              (priority + 1)
              (GameWithPlayback (step ^. #move : previousMoves) (step ^. #game))
