module Solitaire.Core.Moves
  ( FlipCard(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Move.Class (IsMove(..))
import Solitaire.Core.Types (Game, Pile, Tableau)

import qualified Data.Vector as V


type PileOfCards card = Pile (Vector card)

tableauL :: Lens' (Game card foundation stock) (IntMap (PileOfCards card))
tableauL = #tableau . singular #_Tableau

-- | convenience traversal
indexedTableau :: IndexedTraversal' Int (Game card foundation stock) (PileOfCards card)
indexedTableau = tableauL . itraversed


newtype FlipCard = FlipCard
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


instance IsMove FlipCard (Game card foundation stock) where
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
