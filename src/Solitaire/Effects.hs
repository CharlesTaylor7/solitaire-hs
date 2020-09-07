{-# options_ghc -Wwarn #-}
module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

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
              (step ^. step_move : previousMoves, (step ^. step_game))

--  printS $ "Chose move: " ++ pretty move
--  printS "Valid moves:"
--  prettyPrint $ map (view step_move &&& scoreByRuns . view step_game) steps
--  pure steps

{--
runUserInput ::
             ( MonadIO m
             , MonadError GameEnd m
             )
             => Game
             -> m ()
runUserInput game =
  userInput >>= \case
    Left (Input "") -> pure ()
    Right Quit -> throwError gameQuit
    Right Dump -> do
      print game
      runUserInput game
    Left (Input input) -> do
      print $ "Invalid command of: " <> input
      runUserInput game
--}
newGame :: (MonadIO m, MonadReader Config m) => m Game
newGame = do
  shuffled <- getDeck >>= shuffle
  pileCounts <- view config_piles
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
gameIsWon game = game ^. layout . to totalCards . to (== 0)

-- utils
singleton :: a -> [a]
singleton = pure @[]

cozip :: Functor f => Either (f a) (f b) -> f (Either a b)
cozip = fmap Left ||| fmap Right

find :: Monad m => (a -> Bool) -> ListT m a -> MaybeT m a
find predicate producer = do
  (x, prod) <- MaybeT $ next producer
  if predicate x
  then pure x
  else find predicate prod


