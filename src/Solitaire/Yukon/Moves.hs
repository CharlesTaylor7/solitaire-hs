{-# language TypeSynonymInstances, FlexibleInstances #-}
module Solitaire.Yukon.Moves
  ( FlipCard
  , MoveToFoundation
  , MoveStack
  ) where

import Solitaire.Prelude
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (splitAtFirstRun, isSuccessorOf)
import Solitaire.Core.Move.Class
import Solitaire.Core.Moves (FlipCard)

import Solitaire.Yukon.Types

import qualified Data.Vector as V
import qualified Data.IntMap as M

-- import Data.List (inits)


type PileOfCards = Pile (Vector Card)

-- | convenience traversal
indexedTableau :: IndexedTraversal' Int Game PileOfCards
indexedTableau = #tableau . #_Tableau . itraversed

newtype MoveToFoundation = MoveToFoundation
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


safeSucc :: (Eq a, Bounded a, Enum a) => a -> Maybe a
safeSucc val
  | val == maxBound = Nothing
  | otherwise       = Just $ succ val


instance IsMove MoveToFoundation Game where
  steps :: Game -> [(MoveToFoundation, Game)]
  steps game = do
    let
      foundationNeeds :: Card -> Bool
      foundationNeeds card = game
        ^?! #foundation
        . #_Foundation
        . failing
          (ix (card ^. #suit) . to safeSucc . _Just)
          (like Ace)
        . to (is $ only (card ^. #rank))

    -- parse valid cards to move
    (pileId, (card, rest)) <- game
      ^.. indexedTableau
      . cloneIndexPreservingTraversal
        ( #faceUp
        . filteredBy (_head . filtered foundationNeeds)
        . _Cons
        )
      . withIndex

    -- apply update
    pure $ flip runState game $ do
      -- update pile in tableau
      #tableau . #_Tableau . ix pileId . #faceUp
        .= rest

      -- update foundation
      #foundation . #_Foundation . at (card ^. #suit)
        ?= card ^. #rank

      -- descrive game update
      pure $ MoveToFoundation pileId
-------------------------------------
--
data MoveStack = MoveStack
  { fromIndex :: Int
  , toIndex :: Int
  , numMoved :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove MoveStack Game where
  steps :: Game -> [(MoveStack, Game)]
  steps game = do
    let
      targets :: Card -> Fold PileOfCards (Vector Card)
      targets card = failing
          -- can move onto pile when the target pile's first card is the successor of our card
        (#faceUp . filteredBy (_head . filtered (`isSuccessorOf` card)))
        -- can move onto empty piles when the source stack ends with a king
        ( filteredBy (like card . #rank . only King)
        . filteredBy (#faceUp . _Empty)
        . filteredBy (#faceDown . _Empty)
        . like mempty
        )

    -- parse valid source stacks
    (sourcePileId, sourceFaceUp) <- game
      ^@.. indexedTableau
      . cloneIndexPreservingTraversal #faceUp

    (sourceStack, sourceRest) <- [1 .. V.length sourceFaceUp]
      <&>  flip V.splitAt sourceFaceUp

    -- parse stack bottom
    stackBottom <- sourceStack ^.. _last

    -- parse valid targets based on current source stack's bottom card
    (targetPileId, targetFaceUp) <- game
      ^@.. indexedTableau
      <. targets stackBottom

    let
      numMoved = length sourceStack
      updatedFaceUp = sourceStack <> targetFaceUp

    -- apply update to game
    pure $ flip runState game $ do
      zoom (#tableau . #_Tableau) $ do
        ix sourcePileId . #faceUp .= sourceRest
        ix targetPileId . #faceUp .= updatedFaceUp

      -- describe the move
      pure $ MoveStack sourcePileId targetPileId numMoved

