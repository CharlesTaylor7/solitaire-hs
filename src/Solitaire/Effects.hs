{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}
module Solitaire.Effects where

import Solitaire.Imports
import Solitaire.Types
import Solitaire.PrettyPrinter
import Solitaire.Utils
import Solitaire.Actions

runGame :: Env -> IO ()
runGame env =
  flip runReaderT env $
    newGame >>= loopM act

act :: (MonadIO m, MonadReader Env m, MonadRandom m) => Game -> m (Either Game ())
act game = do
  prettyPrint game
  userConfirm
  steps <- validSteps game
  printS "Valid moves:"
  prettyPrint $ map _step_move steps
  next <- randomElem steps
  case next of
    Nothing -> do
      printS "No valid moves"
      pure $ Right ()
    Just (Step move game) -> do
      printS $ "Chose move: " ++ pretty move
      pure $ Left game

newGame :: (MonadIO m, MonadRandom m, MonadReader Env m) => m Game
newGame = do
  deck <- getDeck
  shuffled <- shuffleIO deck
  pileSizes <- getPileSizes
  let
    piles = fst $ foldl'
      (\(ps, cs) size ->
        let (p, cs') = splitAt size cs
        in (toPile p : ps, cs'))
      ([], deck)
      pileSizes
    layout = Layout $ indexFrom 0 piles
    foundation = Foundation 0
  pure $ Game layout foundation
