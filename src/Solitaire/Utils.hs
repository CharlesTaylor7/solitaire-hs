module Solitaire.Utils where

import Solitaire.Imports
import qualified Data.Vector as V

cards :: PileCards -> Vector Card
cards pile = up <> down
  where
    up = pile ^. faceUp
    down = pile ^. faceDown

emptyPile :: PileCards
emptyPile = Pile V.empty V.empty

getDeck :: MonadReader Env m => m [Card]
getDeck = do
  numSets <- view env_numSets
  pure $ enumerate >>= replicate numSets

pileCountsSize :: PileCounts -> Int
pileCountsSize counts =
  counts ^. faceUp +
  counts ^. faceDown

toPile :: [Card] -> PileCounts -> PileCards
toPile cards counts =
  let
    n = counts ^. faceUp
    (up, down) = splitAt n cards
  in
    Pile (V.fromList up) (V.fromList down)
