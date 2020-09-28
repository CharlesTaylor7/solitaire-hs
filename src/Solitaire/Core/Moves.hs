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


type PileOfCards card = Pile (Vector card)

-- | convenience traversal
indexedTableau :: IndexedTraversal' Int Game (PileOfCards Card)
indexedTableau = #tableau . #_Tableau . itraversed


newtype FlipCard = FlipCard
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


-- type HasSimpleField symbol s a = HasField symbol s s a a
instance (Generic game, HasField' "tableau" game (Tableau card)) => IsMove FlipCard game where
  steps game =
    game ^.. indexedTableau
    . to flipCard
    . _Just
    . withIndex
    . to
      (   (FlipCard . fst)
      &&& \(pileId, pile) -> game & #tableau . #_Tableau . ix pileId .~ pile
      )
    where
      flipCard :: PileOfCards card -> Maybe (PileOfCards card)
      flipCard pile
        -- face down pile is covered by face up cards
        | pile ^. #faceUp . to (not . V.null) = Nothing
        | otherwise =
          pile ^? #faceDown . _Cons .
            to
              (\(card, rest) ->
                pile
                  & #faceUp .~ V.singleton card
                  & #faceDown .~ rest
              )

--
