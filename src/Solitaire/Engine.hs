module Solitaire.Engine where

import Solitaire.Prelude
import Solitaire.Types
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.RuleSet

-- types
type PriorityQueuePayload = ([Move], Game)

newtype App a = App
  { unApp ::  (PQueueT MoveCount PriorityQueuePayload (HistoryT Game (ReaderT Config IO))) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadHistory Game
    , MonadPQueue MoveCount PriorityQueuePayload
    , MonadReader Config
    )

data UserInput = Quit | Dump
  deriving (Eq, Show, Read)

data GameConclusion = GameWon [Move] | GameLost
  deriving (Eq, Show)

data GameQuit = UserQuit
  deriving Show

newtype MoveCount = MoveCount Int
  deriving (Eq, Ord, Num)

-- convenience constructors
gameWon :: [Move] -> GameConclusion
gameWon =  GameWon

gameLost :: GameConclusion
gameLost =  GameLost

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
  case result  of
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

newGame :: (MonadIO m, MonadReader Config m) => m Game
newGame = do
  shuffled <- getDeck >>= shuffle
  pileCounts <- view #piles
  let
    piles = fst $ foldl'
      (\(ps, cs) count ->
        let
          size = pileCountsSize count
          (p, cs') = splitAt size cs
        in
          (toPile p count : ps, cs'))
      ([], shuffled)
      pileCounts
    layout = Layout $ indexFrom 0 $ reverse piles
    foundation = Foundation 0
  pure $ Game layout foundation

gameIsWon :: Game -> Bool
gameIsWon game = game ^. #layout . to totalCards . to (== 0)
