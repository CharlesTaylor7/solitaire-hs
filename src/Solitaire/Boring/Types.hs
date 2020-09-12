{-# options_ghc -Wno-orphans #-}
module Solitaire.Boring.Types
  ( module CoreTypes
  , Card(..)
  , Config(..)
  , PileCards
  , Game(..)
  , Layout(..)
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
  )


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)


type PileCards = Pile (Vector Card)

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Ord, Show, Generic)


data Foundation = Foundation
  { numSets :: Int
  }
  deriving (Eq, Ord, Show, Generic)

data Game = Game
  { layout :: Layout
  , foundation :: Foundation
  }
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Show, Generic)

instance Exception InvalidMove

-- hashable instances
instance Hashable Game
instance Hashable Layout
instance Hashable Foundation
instance Hashable Card
instance Hashable a => Hashable (Pile a)


flipCard :: Int -> Move
flipCard = FlipCard . FC

moveToFoundation :: Int -> Move
moveToFoundation = MoveToFoundation . MTF

moveStack :: Int -> Int -> Move
moveStack = (MoveStack .) . MS
