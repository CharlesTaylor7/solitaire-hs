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
  steps game =
    let
      sourceStacks :: IndexedFold Int Game (Vector Card, Vector Card)
      sourceStacks = indexedTableau <. #faceUp . to splitAtFirstRun

      targets :: Card -> IndexedFold Int Game (Vector Card)
      targets card = indexedTableau <.
        failing
          (#faceUp . filteredBy (_head . filtered (`isSuccessorOf` card)))
          (filteredBy (#faceUp . _Empty) . filteredBy (#faceDown . _Empty) . like mempty)

    in do
      (source_i, (sourceStack, sourceRest)) <- game ^.. sourceStacks . withIndex

      stackBottom <- sourceStack ^.. _last

      (target_i, targetFaceUp) <- game ^.. targets stackBottom . withIndex

      let
        numMoved = length sourceStack
        updatedFaceUp = sourceStack <> targetFaceUp
        game' = game
            & #tableau . #_Tableau . ix target_i . #faceUp .~ updatedFaceUp
            & #tableau . #_Tableau . ix source_i . #faceUp .~ sourceRest

      pure $ (MoveStack source_i target_i numMoved, game')
