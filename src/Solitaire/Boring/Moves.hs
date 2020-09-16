{-# language FlexibleInstances #-}
module Solitaire.Boring.Moves
  ( MoveToFoundation
  , FlipCard
  , MoveStack
  ) where

import Solitaire.Prelude
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (splitAtFirstRun)
import Solitaire.Core.Move

import Solitaire.Boring.Types

import qualified Data.Vector as V
import qualified Data.IntMap as M

-- | convenience traversal
indexedTableau :: IndexedTraversal' Int Game (Int, Pile (Vector Card))
indexedTableau = #tableau . #_Tableau . itraversed . withIndex

newtype MoveToFoundation = MoveToFoundation
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


instance IsMove MoveToFoundation Game where
  steps :: Game -> [(MoveToFoundation, Game)]
  steps game =
    game ^.. indexedTableau
    . to (_2 %~ takeSet)
    . aside _Just
    . to
      (   (MoveToFoundation . fst)
      &&& \(pileId, pile) ->
            game
            & #tableau . #_Tableau . ix pileId .~ pile
            & #foundation . #numSets +~ 1
      )
    where
      takeSet :: Pile (Vector Card) -> Maybe (Pile (Vector Card))
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
    . to (_2 %~ flipCard)
    . aside _Just
    . to
      (   (FlipCard . fst)
      &&& \(pileId, pile) -> game & #tableau . #_Tableau . ix pileId .~ pile
      )
    where
      flipCard :: Pile (Vector Card) -> Maybe (Pile (Vector Card))
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
data MoveStack = MS
  { fromIndex :: Int
  , toIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove MoveStack Game where

{--
  moveReducer
    :: (MonadError Boring.InvalidMove m)
    => Boring.Move
    -> Boring.Game
    -> m Boring.Game
  moveReducer move =
      MoveStack (MS i j) ->
        #tableau . #_Tableau $ \tableau -> do
          let
            (stack, sourcePileRest) =
              tableau ^?! ix i . #faceUp . to splitAtFirstRun
            target = tableau ^?! ix j

          -- sanity checking
          when (i == j) $
            throwError $ SourceIsTarget i

          when (null stack) $
            throwError $ EmptyStackSource i

          -- short circuit case where you move a stack onto an empty pile
          if V.null . cards $ target
          then error "todo"
          else do
            -- Can never move a stack onto facedown cards
            when (is _Empty $ target ^. #faceUp) $
              throwError $ MoveStackOntoFaceDownCards  j

          let targetCard = tableau ^?! ix j . #faceUp . _head
          let stackBottomCard = stack ^?! _last
          let stackSize = length stack
          let diff = fromEnum targetCard - fromEnum stackBottomCard
          let splitStackAt = length stack - diff - 1

          when (splitStackAt < 0 || splitStackAt >= stackSize) $
            throwError $ MismatchingStacks i j
              & traceShow (splitStackAt, stack)

          let
            (stackPartToMove, stackPartToLeave) = V.splitAt splitStackAt stack

            sourceUpdate = ix i . #faceUp .~ (stackPartToLeave <> sourcePileRest)
            targetUpdate = ix j . #faceUp %~ (stackPartToMove <>)

          pure $ tableau & sourceUpdate . targetUpdate
--}
