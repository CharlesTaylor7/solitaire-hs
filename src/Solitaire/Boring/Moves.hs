{-# language FlexibleInstances #-}
module Solitaire.Boring.Moves
  ( MoveToFoundation
  , FlipCard
  , MoveStack
  ) where

import Solitaire.Prelude
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (splitAtFirstRun, isSuccessorOf)
import Solitaire.Core.Move

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


-------------------------------------
newtype FlipCard = FlipCard
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove FlipCard Game where
  steps :: Game -> [(FlipCard, Game)]
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
      flipCard :: PileOfCards -> Maybe (PileOfCards)
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

-------------------------------------
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
      sourceStacks :: IndexedFold Int Game (Vector Card, Card, Vector Card)
      sourceStacks =
        indexedTableau
        <. #faceUp
        . to splitAtFirstRun
        . to
          (\(stack, rest) ->
            stack ^? _Snoc . to (\(run, card) -> (run, card, rest))
          )
        . _Just

      targets :: Card -> IndexedFold Int Game (Vector Card)
      targets card = indexedTableau . #faceUp . to (isTarget card) . _Just

      isTarget :: Card -> Vector Card -> Maybe (Vector Card)
      isTarget card cards = cards ^? to (id &&& alongside id _head . filtered (card `isSuccessorOf`)

    in do
      -- parse, don't validate
      (source_i, (sourceStack, stackBottom, sourceRest)) <- game ^.. sourceStacks . withIndex

      (target_i, targetFaceUp) <- game ^.. targets stackBottom . withIndex

      let
        numMoved = length sourceStack
        updatedFaceUp = sourceStack <> cons stackBottom targetFaceUp
        game' = game
            & #tableau . #_Tableau . ix target_i . #faceUp .~ updatedFaceUp
            & #tableau . #_Tableau . ix source_i . #faceUp .~ sourceRest

      pure $ (MoveStack source_i target_i numMoved, game')
