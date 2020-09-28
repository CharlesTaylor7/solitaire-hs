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
  steps game = do
    (pileId, (card, rest)) <- game
      ^.. (indexedTableau <. filteredBy (#faceUp . _Empty))
      . cloneIndexPreservingTraversal (#faceDown . _Cons)
      . withIndex

    pure $ flip runState game $ do
      zoom (tableauL . ix pileId) $ do
        #faceUp .= V.singleton card
        #faceDown .= rest

      pure $ FlipCard pileId
