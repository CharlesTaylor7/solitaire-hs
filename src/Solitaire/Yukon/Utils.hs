module Solitaire.Yukon.Utils where

import Solitaire.Prelude
import Solitaire.Yukon.Types

import qualified Data.Vector as V

cards :: PileCards -> Vector Card
cards pile = pile ^. #faceUp <> pile ^. #faceDown

emptyPile :: PileCards
emptyPile = Pile V.empty V.empty

getDeck :: MonadReader Config m => m [Card]
getDeck = do
  numSets <- view #numSets
  pure $ enumerate >>= replicate numSets

pileCountsSize :: PileCounts -> Int
pileCountsSize counts = counts ^. #faceUp + counts ^. #faceDown

toPile :: [Card] -> PileCounts -> PileCards
toPile cards counts =
  let
    n = counts ^. #faceUp
    (up, down) = splitAt n cards
  in
    Pile (V.fromList up) (V.fromList down)

pileSize :: PileCards -> Int
pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)

totalCards :: Layout -> Int
totalCards = sumOf (#_Layout . folded . to pileSize)
