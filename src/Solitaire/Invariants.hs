module Solitaire.Invariants where

import Solitaire.Imports

import GHC.Exts (IsList(..))


pileSize :: PileCards -> Int
pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)

cardsInPile :: PileCards -> Set Card
cardsInPile pile =
  (pile ^.. #faceUp . folded & fromList) <>
  (pile ^.. #faceDown . folded & fromList)

totalCards :: Layout -> Int
totalCards = alaf Sum foldMap pileSize . unLayout

cardsInLayout :: Layout -> Set Card
cardsInLayout = foldMap cardsInPile . unLayout
