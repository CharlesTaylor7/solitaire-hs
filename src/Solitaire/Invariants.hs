module Solitaire.Invariants where

import Solitaire.Imports
import Solitaire.Types
import qualified Data.Set as Set
import qualified Data.Vector as V

pileSize :: Pile -> Int
pileSize = (+) <$> V.length . view faceUp <*> V.length . view faceDown

cardsInPile :: Pile -> Set Card
cardsInPile =
  Set.fromList . (
    (V.toList . view faceUp) <>
    (V.toList . view faceDown)
  )

totalCards :: Layout -> Int
totalCards = alaf Sum foldMap pileSize . unLayout

cardsInLayout :: Layout -> Set Card
cardsInLayout = foldMap cardsInPile . unLayout
