{-# language FlexibleInstances #-}
module Solitaire.Boring.Moves
  ( FlipCard
  , MoveToFoundation
  , MoveStack
  ) where

import Solitaire.Prelude
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (splitAtFirstRun, isSuccessorOf)
import Solitaire.Core.Move.Class
import Solitaire.Core.Moves (FlipCard)

import Solitaire.Boring.Types

import qualified Data.Vector as V
import qualified Data.IntMap as M



type PileOfCards = Pile (Vector Card)

-- | convenience traversal
indexedTableau :: IndexedTraversal' Int Game PileOfCards
indexedTableau = #tableau . #_Tableau . itraversed

newtype MoveToFoundation = MoveToFoundation
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


instance IsMove MoveToFoundation Game where
  steps :: Game -> [(MoveToFoundation, Game)]
  steps game =
    game ^.. indexedTableau
    . to takeSet
    . _Just
    . withIndex
    . to
      (   (MoveToFoundation . fst)
      &&& \(pileId, pile) ->
            game
            & #tableau . #_Tableau . ix pileId .~ pile
            & #foundation . #numSets +~ 1
      )
    where
      takeSet :: PileOfCards -> Maybe (PileOfCards)
      takeSet pile =
        pile ^. #faceUp
        & splitAtFirstRun
        & \(run, rest) ->
          if length run == enumSize @Card
          then Just $ pile & #faceUp .~ rest
          else Nothing


------------------------------------
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
        -- can move onto empty piles
        (filteredBy (#faceUp . _Empty) . filteredBy (#faceDown . _Empty) . like mempty)

    -- parse valid source stacks
    (source_i, (sourceStack, sourceRest)) <- game
      ^.. (indexedTableau <. #faceUp)
      . to splitAtFirstRun
      . withIndex

    -- parse stack bottom
    stackBottom <- sourceStack ^.. _last

    -- parse valid targets based on current source stack's bottom card
    (target_i, targetFaceUp) <- game
      ^.. (indexedTableau <. targets stackBottom)
      . withIndex

    let
      numMoved = length sourceStack
      updatedFaceUp = sourceStack <> targetFaceUp

    -- apply update to game
    pure $ flip runState game $ do
      zoom (#tableau . #_Tableau) $ do
        ix source_i . #faceUp .= sourceRest
        ix target_i . #faceUp .= updatedFaceUp

      -- describe the move
      pure $ MoveStack source_i target_i numMoved
