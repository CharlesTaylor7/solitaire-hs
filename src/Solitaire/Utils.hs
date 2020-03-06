module Solitaire.Utils where

import Solitaire.Imports
import Solitaire.Types
import qualified Data.Vector as V

cards :: Pile -> Vector Card
cards pile = up <> down
  where
    up = pile ^. faceUp
    down = pile ^. faceDown

emptyPile :: Pile
emptyPile = Pile V.empty V.empty

getPileSizes :: MonadReader Env m => m [Int]
getPileSizes = do
  p <- view env_numPiles
  s <- view env_numSets
  let
    n = enumSize @Card
    (q, r) = (s * n) `divMod` p
    piles = replicate (p - r) q ++ replicate r (q + 1)
  pure piles

getDeck :: MonadReader Env m => m [Card]
getDeck = do
  numSets <- view env_numSets
  pure $ enumerate >>= replicate numSets

toPile :: MonadReader Env m => [Card] -> m Pile
toPile cards = do
  numFaceUp <- view env_numFaceUpPerPile
  let
    (faceUp, faceDown) = splitAt numFaceUp cards
    pile = Pile (V.fromList faceUp) (V.fromList faceDown)
  pure pile
