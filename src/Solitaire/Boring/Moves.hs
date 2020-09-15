{-# language FlexibleInstances #-}
{-# options_ghc -Wwarn #-}
module Solitaire.Boring.Moves where

import Solitaire.Prelude
-- import Solitaire.Core.Rules
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Move

import Solitaire.Boring.Types

import qualified Data.Vector as V
import qualified Data.IntMap as M


newtype FlipCard = FC
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove FlipCard Game where
  data InvalidMove FlipCard
    = CardFlipOnUnexposedPile Int
    | CardFlipOnEmptyPile Int
      deriving stock (Eq, Show, Generic)
      deriving anyclass (Exception)

-------------------------------------
data MoveStack = MS
  { fromIndex :: Int
  , toIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove MoveStack Game where
  data InvalidMove MoveStack
    = MismatchingStacks Int Int
    | EmptyStackSource Int
    | MoveStackOntoFaceDownCards Int
    | SourceIsTarget Int
      deriving stock (Eq, Show, Generic)
      deriving anyclass (Exception)


-------------------------------------
newtype MoveToFoundation = MTF
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance IsMove MoveToFoundation Game where

  data InvalidMove MoveToFoundation = IncompleteSet Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Exception)
{--
  moves :: NumPiles -> [Boring.Move]
  moves (NumPiles numPiles) =
    let
      range = [0..numPiles-1]
      moves = moveStack <$> range <*> range
      flips = flipCard <$> range
      sets = moveToFoundation <$> range
    in
      sets <> flips <> moves


  moveReducer
    :: (MonadError Boring.InvalidMove m)
    => Boring.Move
    -> Boring.Game
    -> m Boring.Game
  moveReducer move =
    case move of
      FlipCard (FC i) ->
        #tableau . #_Tableau . ix i $ \pile -> do
          if is _Just $ pile ^? #faceUp . _Cons
          then throwError (CardFlipOnUnexposedPile i)
          else pure ()

          (head, rest) <- pile ^? #faceDown . _Cons
            & (maybeToError $ CardFlipOnEmptyPile i)

          let faceUp' = #faceUp .~ [head]
          let faceDown' = #faceDown .~ rest
          pure $ pile & faceUp' . faceDown'
      MoveToFoundation (MTF i) ->
        #foundation . #numSets +~ 1 >>>
        (#tableau . #_Tableau . ix i $ \pile ->
          let
            (set, leftover) = pile ^. #faceUp . to splitAtFirstRun
            pile' = pile & #faceUp .~ leftover
          in do
            when (length set /= enumSize @Boring.Card) $ throwError $ IncompleteSet i
            pure pile'
        )
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


