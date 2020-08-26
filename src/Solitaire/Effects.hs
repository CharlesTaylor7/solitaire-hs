module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions


find :: Monad m => (a -> Bool) -> ListT m a -> MaybeT m a
find predicate producer = do
  (x, prod) <- MaybeT $ next producer
  if predicate x
  then pure x
  else find predicate prod

newtype LoopMonad a = LoopMonad
  { unLoopMonad :: ListT (ExceptT GameQuit (StateT (Set Game) (ReaderT Config IO))) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCache Game
    , MonadReader Config
    , MonadError GameQuit
    )

instance MonadRandom LoopMonad where
  getRandomR = LoopMonad . lift . getRandomR
  getRandom = LoopMonad $ lift getRandom
  getRandomRs = LoopMonad . lift . getRandomRs
  getRandoms = LoopMonad $ lift getRandoms

runGame :: Config -> IO ()
runGame config =
  let
    runGameLoop :: LoopMonad GameConclusion
    runGameLoop = do
      game <- newGame
      let
        fakeMove = moveStack 0 0
        step = Step fakeMove game
      loopM (surgery . act) step
  in do
    result <-
      flip runReaderT config .
      flip evalStateT mempty .
      runExceptT .
      runMaybeT .
      find (== GameWon) $
        runGameLoop
    case result of
      Left quit -> print quit
      Right (Just won) -> print gameWon
      Right Nothing -> print gameLost

data UserInput = Quit | Dump
  deriving (Eq, Show, Read)

data GameEnd = GameConclusion GameConclusion | GameQuit GameQuit
  deriving Show
data GameConclusion = GameWon | GameLost
  deriving (Eq, Show)
data GameQuit = UserQuit
  deriving Show

gameWon, gameLost, gameQuit :: GameEnd
gameWon = GameConclusion GameWon
gameLost = GameConclusion GameLost
gameQuit = GameQuit UserQuit

surgery :: Monad m
        => ExceptT GameEnd m [a]
        -> ListT (ExceptT GameQuit m) (Either GameConclusion a)
surgery = weaveList . separateErrors

separateErrors :: Functor m
               => ExceptT GameEnd m a
               -> ExceptT GameQuit m (Either GameConclusion a)
separateErrors ex = ExceptT $ splitGameEnd <$> runExceptT ex
  where
    splitGameEnd :: Either GameEnd a -> Either GameQuit (Either GameConclusion a)
    splitGameEnd = \case
      Left (GameConclusion conclusion) -> Right . Left $ conclusion
      Left (GameQuit quit) -> Left quit
      Right y -> Right . Right $ y

weaveList :: Monad m
         => m (Either GameConclusion [a])
         -> ListT m (Either GameConclusion a)
weaveList = listT . fmap distribute
  where
    distribute :: Either a [b] -> [Either a b]
    distribute = uncozip . first singleton

singleton :: a -> [a]
singleton = pure @[]

uncozip :: Functor f => Either (f a) (f b) -> f (Either a b)
uncozip = fmap Left ||| fmap Right

act ::
    ( MonadIO m
    , MonadReader Config m
    , MonadError GameEnd m
    , MonadCache Game m
    )
    => Step
    -> m [Step]
act (Step move game) = do
  saveToCache game
  printS $ "Chose move: " ++ pretty move
  prettyPrint game
  when (gameIsWon game) $
    throwError gameWon
  runUserInput game
  steps <- nextSteps game
  when (null steps) $
    throwError gameLost
  printS "Valid moves:"
  prettyPrint $ map (view step_move &&& scoreByRuns . view step_game) steps
  pure steps

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
