module Solitaire.Engine where

import Solitaire.Prelude
import Solitaire.PrettyPrinter
import Solitaire.RuleSet


runGameLoop :: App (Game, GameConclusion)
runGameLoop = do
  game <- newGame
  queueInsert 0 ([], game)
  conclusion <- App $ loopM (\_ -> step) ()
  pure (game, conclusion)

runGame :: Config -> IO ()
runGame config = do
  result <- runGameLoop
    & unApp
    & runPQueueT
    & runHistoryT
    & flip runReaderT config
  case result of
    (game, GameLost) -> do
      prettyPrint game

      putStrLn "GameLost"

    (game, GameWon moves) -> do
      prettyPrint game
      liftIO $ putStrLn ""
      throwInIO $ void $ foldM observeGameStep game (reverse moves)
      putStrLn "GameWon"

throwInIO :: (MonadIO m, Exception e) => ExceptT e m a -> m a
throwInIO = join . fmap rightOrThrow . runExceptT

observeGameStep
  :: (MonadError InvalidMove m, MonadIO m)
  => Game
  -> Move
  -> m Game
observeGameStep game move = do
  game <- moveReducer move game
  prettyPrint game
  liftIO $ putStrLn ""
  pure game

step
  ::
  ( MonadIO m
  , MonadReader Config m
  , MonadError GameConclusion m
  , MonadHistory Game m
  , MonadPQueue MoveCount PriorityQueuePayload m
  )
  => m ()
step = do
  -- retrieve the best priority game state
  maybeMin <- queuePopMin
  case maybeMin of
    -- out of game states to try, we lost
    Nothing ->
      throwError gameLost

    -- we have game states to play from
    Just (priority, (previousMoves, game)) -> do
      -- game states can be reached multiple times via different paths
      -- verify we haven't visted this state before
      visited <- historyHas game
      when (not visited) $ do

        -- check to see if we won
        when (gameIsWon game) $
          throwError $ gameWon previousMoves

        -- record we visited this game state
        saveToHistory game
        -- checking the history hash set is much cheaper than spurious extra inserts to the priority queue
        -- so we check the visited set both at insert time & queue pop time
        steps <- nextSteps game

        -- insert new game states reachable from this one
        for_ steps $ \step -> do
            queueInsert
              (priority + 1)
              (step ^. #move : previousMoves, (step ^. #game))

