module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

yieldFirst :: Monad m => ListT m a -> m (Maybe a)
yieldFirst (Select producer) = do
  either <- next producer
  pure $ either ^? _Right . _1

runGame :: Config -> IO ()
runGame config =
  let
    runGameLoop = do
      game <- newGame
      let
        fakeMove = moveStack 0 0
        step = Step fakeMove game
      loopM' act step
  in do
    ending <- runExceptT . flip runReaderT config . yieldFirst $ runGameLoop
    let
      gameEnd = ending ^?! _Right . _Just
    print @_ @GameEnd gameEnd

data GameEnd = GameWon | GameLost
  deriving (Eq, Show, Read)

instance Exception GameEnd

act :: (MonadIO m, MonadReader Config m, MonadError GameEnd m) => Step -> ListT m Step
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
  prettyPrint $ map (view step_move &&&  scoreByRuns . view step_game) steps
  Select $ each steps

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
