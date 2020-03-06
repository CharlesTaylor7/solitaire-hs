module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Invariants
import Solitaire.Types
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

runGame :: Env -> IO ()
runGame env = do
  gameEnd <- flip runReaderT env $
    newGame >>= loopM (runExceptT . act)
  print gameEnd

data GameEnd = GameWon | GameLost
  deriving (Eq, Show, Read)

act :: (MonadIO m, MonadRandom m, MonadReader Env m, MonadError GameEnd m) => Game -> m Game
act game = do
  prettyPrint game
  if gameWon game
    then throwError GameWon
    else pure ()
  userConfirm
  steps <- validSteps game
  printS "Valid moves:"
  prettyPrint $ map _step_move steps
  next <- randomElem steps
  case next of
    Nothing -> do
      throwError GameLost
    Just (Step move game) -> do
      printS $ "Chose move: " ++ pretty move
      pure game

newGame :: (MonadIO m, MonadRandom m, MonadReader Env m) => m Game
newGame = do
  shuffled <- getDeck >>= shuffleIO
  pileCounts <- view env_piles
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
