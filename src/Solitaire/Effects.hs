module Solitaire.Effects where

import Solitaire.Imports
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

runGame :: Config -> IO ()
runGame config =
  let
    runGameLoop :: ListT (ReaderT Config IO) GameEnd
    runGameLoop = do
      game <- newGame
      let
        fakeMove = moveStack 0 0
        step = Step fakeMove game
      loopM @LoopMonad (harderSurgery . act) step
  in do
    Just gameEnd <- flip runReaderT config . find (== GameWon) $ runGameLoop
    print @_ @GameEnd gameEnd

data GameEnd = GameWon | GameLost
  deriving (Eq, Show, Read)

instance Exception GameEnd

type LoopMonad = ListT (ReaderT Config IO)

harderSurgery :: ExceptT GameEnd (ReaderT Config IO) [Step]
              -> ListT (ReaderT Config IO) (Either GameEnd Step)
harderSurgery = listT . fmap surgery . runExceptT

surgery :: Either a [b] -> [Either a b]
surgery = uncozip . first singleton

singleton :: a -> [a]
singleton = pure @[]

uncozip :: Functor f => Either (f a) (f b) -> f (Either a b)
uncozip = fmap Left ||| fmap Right

act :: (MonadIO m, MonadReader Config m, MonadError GameEnd m) => Step -> m [Step]
act (Step move game) = do
  printS $ "Chose move: " ++ pretty move
  prettyPrint game
  ifThenError (gameWon game) $
    GameWon
  userConfirm
  steps <- validSteps game
  ifThenError (null steps) $
    GameLost
  printS "Valid moves:"
  prettyPrint $ map (view step_move &&& scoreByRuns . view step_game) steps
  pure steps

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
