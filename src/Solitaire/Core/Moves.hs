{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Solitaire.Core.Moves
  ( FlipCard(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (splitAtFirstRun, isSuccessorOf)
import Solitaire.Core.Move.Class

import Solitaire.Boring.Types

import qualified Data.Vector as V
import qualified Data.IntMap as M

import Data.Generics.Product.Fields
import Data.Generics.Product


type PileOfCards card = Pile (Vector card)

tableauL :: forall game card. HasField' "tableau" game (Tableau card) => Lens' game (IntMap (PileOfCards card))
tableauL = field' @"tableau" @game @(Tableau card) . singular #_Tableau

-- | convenience traversal
indexedTableau :: forall game card. HasField' "tableau" game (Tableau card) => IndexedTraversal' Int game (PileOfCards card)
indexedTableau = tableauL . itraversed


newtype FlipCard = FlipCard
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


instance (HasField' "tableau" game (Tableau card)) => IsMove FlipCard game where
  steps game =
    game ^.. faceDownPiles
    . to flipCard
    . _Just
    . withIndex
    . to
      (   (FlipCard . fst)
      &&& \(pileId, pile) -> game & tableauL . ix pileId .~ pile
      )
    where
      faceDownPiles :: IndexedTraversal' Int game (PileOfCards card)
      faceDownPiles = indexedTableau <. filteredBy (#faceUp . _Empty)

      flipCard :: PileOfCards card -> Maybe (PileOfCards card)
      flipCard pile =
        pile ^? #faceDown . _Cons .
          to
            (\(card, rest) ->
              pile
                & #faceUp .~ V.singleton card
                & #faceDown .~ rest
            )

--
