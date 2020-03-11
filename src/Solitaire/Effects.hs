module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

runGame :: Config -> IO ()
runGame config = do
  gameEnd <- flip runReaderT config . _ $ do
    game <- newGame
    let
      fakeMove = moveStack 0 0
      step = Step fakeMove game
    loopM (runExceptT . act) step
  print gameEnd

data GameEnd = GameWon | GameLost
  deriving (Eq, Show, Read)

fromList :: Monad m => [b] -> ListT m b
fromList = Select . mapM_ yield

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
  prettyPrint $ map (view step_move) steps
  fromList steps

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
    layout = Layout $ indexFrom 0 piles
    foundation = Foundation 0
  pure $ Game layout foundation

gameWon :: Game -> Bool
gameWon game = game ^. layout . to totalCards . to (== 0)
