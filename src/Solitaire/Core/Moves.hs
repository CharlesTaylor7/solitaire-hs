{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Solitaire.Core.Moves
  ( FlipCard(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Move.Class (IsMove(..))
import Solitaire.Core.Types (Pile, Tableau)

import qualified Data.Vector as V


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
    -- parse valid piles
    (pileId, (card, rest)) <- game
      ^.. (indexedTableau <. filteredBy (#faceUp . _Empty))
      . cloneIndexPreservingTraversal (#faceDown . _Cons)
      . withIndex

    -- generate state change based on pile info
    pure $ flip runState game $ do
      zoom (tableauL . ix pileId) $ do
        #faceUp .= V.singleton card
        #faceDown .= rest

      -- describe the state change
      pure $ FlipCard pileId
