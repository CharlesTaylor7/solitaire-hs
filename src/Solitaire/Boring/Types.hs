{-# options_ghc -Wno-orphans #-}
module Solitaire.Boring.Types
  ( module CoreTypes
  , Card(..)
  , Config(..)
  , Game(..)
  , Foundation(..)
  , Move(..)
  , InvalidMove(..)
  , MoveStack(..)
  , MoveToFoundation(..)
  , FlipCard(..)
  , moveStack
  , moveToFoundation
  , flipCard
  ) where

import Solitaire.Prelude
import Solitaire.Core.Types as CoreTypes
  ( Pile(..)
  , Score(..)
  , Config(..)
  , Tableau(..)
  )

import Solitaire.Core.Card (IsCard(..))


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance IsCard Card where
  a `isSuccessorOf` b =
    fromEnum a - fromEnum b == 1


data Foundation = Foundation
  { numSets :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


data Game = Game
  { tableau :: Tableau Card
  , foundation :: Foundation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)


-- TODO: use open sum type?
data Move
  = MoveStack MoveStack
  | FlipCard FlipCard
  | MoveToFoundation MoveToFoundation
  deriving (Eq, Show, Generic)

data MoveStack = MS
  { fromIndex :: Int
  , toIndex :: Int
  }
  deriving (Eq, Show, Generic)

newtype FlipCard = FC
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)

newtype MoveToFoundation = MTF
  { pileIndex :: Int
  }
  deriving (Eq, Show, Generic)


data InvalidMove
  = CardFlipOnUnexposedPile Int
  | CardFlipOnEmptyPile Int
  | IncompleteSet Int
  | MismatchingStacks Int Int
  | EmptyStackSource Int
  | MoveStackOntoFaceDownCards Int
  | SourceIsTarget Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)


flipCard :: Int -> Move
flipCard = FlipCard . FC


moveToFoundation :: Int -> Move
moveToFoundation = MoveToFoundation . MTF


moveStack :: Int -> Int -> Move
moveStack = (MoveStack .) . MS
