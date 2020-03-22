module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Internal.OrdOrphans
import Solitaire.Invariants
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

find :: Monad m => (a -> Bool) -> ListT m a -> m (Maybe a)
find predicate (Select producer) = do
  either <- next producer
  case either of
    Left _ -> pure Nothing
    Right (x, prod) ->
      if predicate x
        then pure . Just $ x
        else find predicate (Select prod)

type LoopMonad = ListT (ReaderT Config IO)

runGame :: Config -> IO ()
runGame config =
  let
    runGameLoop :: LoopMonad GameEnd
    runGameLoop = do
      game <- newGame
      let
        fakeMove = moveStack 0 0
        step = Step fakeMove game
        surgery =
          weaveList .
          flip evalStateT mempty
      loopM @LoopMonad (surgery . act) step
  in do
    result <-
      flip runReaderT config .
      find (== GameWon) $
        runGameLoop
    print result

data UserInput = Quit | Dump
  deriving (Eq, Show, Read)

data GameEnd = GameWon | GameLost | GameQuit
  deriving (Eq, Show, Read)

instance Exception GameEnd

weaveList :: Monad m
              => ExceptT e m [a]
              -> ListT m (Either e a)
weaveList = listT . fmap distribute . runExceptT
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
  when (gameWon game) $
    throwError GameWon
  runUserInput game
  steps <- nextSteps game
  when (null steps) $
    throwError GameLost
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
    Right Quit -> throwError GameQuit
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

gameWon :: Game -> Bool
gameWon game = game ^. layout . to totalCards . to (== 0)
