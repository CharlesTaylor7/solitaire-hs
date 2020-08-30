{-# options_ghc -Wwarn #-}
module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.PrettyPrinter ()
import Solitaire.Utils
import Solitaire.Actions ()

-- types
newtype App a = App
  { unApp :: ExceptT GameQuit (PQueueT MoveCount Game (StateT (Set Game) (ReaderT Config IO))) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadHistory Game
    , MonadPQueue MoveCount Game
    , MonadReader Config
    , MonadError GameQuit
    , MonadRandom
    )

data UserInput = Quit | Dump
  deriving (Eq, Show, Read)

data GameEnd = GameConclusion GameConclusion | GameQuit GameQuit
  deriving Show
data GameConclusion = GameWon | GameLost
  deriving (Eq, Show)
data GameQuit = UserQuit
  deriving Show

newtype MoveCount = MoveCount Int
  deriving (Eq, Ord)

-- convenience constructors
gameWon, gameLost, gameQuit :: GameEnd
gameWon = GameConclusion GameWon
gameLost = GameConclusion GameLost
gameQuit = GameQuit UserQuit

runGameLoop :: App GameConclusion
runGameLoop = do
  game <- newGame
  loopM (\_ -> App . separateErrors $ step) ()

runGame :: Config -> IO ()
runGame config = do
  result <- runGameLoop
    & unApp
    & runExceptT
    & runPQueueT
    & flip evalStateT mempty
    & flip runReaderT config
  case result of
    Left quit -> print quit
    Right (gameConclusion) -> print gameWon
    Right _ -> print gameLost

step
  ::
  ( MonadIO m
  , MonadReader Config m
  , MonadError GameEnd m
  , MonadHistory Game m
  , MonadPQueue MoveCount Game m
  )
  => m ()
step = pure ()
--  saveToHistory game
--  printS $ "Chose move: " ++ pretty move
--  prettyPrint game
--  when (gameIsWon game) $
--    throwError gameWon
--  runUserInput game
--  steps <- nextSteps game
--  when (null steps) $
--    throwError gameLost
--  printS "Valid moves:"
--  prettyPrint $ map (view step_move &&& scoreByRuns . view step_game) steps
--  pure steps

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

newGame :: (MonadIO m, MonadRandom m, MonadReader Config m) => m Game
newGame = do
  shuffled <- getDeck >>= shuffleIO
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

separateErrors
  :: Functor m
  => ExceptT GameEnd m a
  -> ExceptT GameQuit m (Either GameConclusion a)
separateErrors ex = ExceptT $ splitGameEnd <$> runExceptT ex
  where
    splitGameEnd :: Either GameEnd a -> Either GameQuit (Either GameConclusion a)
    splitGameEnd = \case
      Left (GameConclusion conclusion) -> Right . Left $ conclusion
      Left (GameQuit quit) -> Left quit
      Right y -> Right . Right $ y

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


