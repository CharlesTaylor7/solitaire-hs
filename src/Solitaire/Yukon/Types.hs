{-# options_ghc -Wno-orphans #-}
module Solitaire.Yukon.Types
  ( module CoreTypes
  , Config(..)
  , PileCounts
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


data Config = Config
  { numSets :: Int
  , piles :: IntMap PileCounts
  }
  deriving (Eq, Show, Read, Generic)

type PileCounts = Pile Int
type PileCards = Pile (Vector Card)

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Show, Generic)

data Foundation = Foundation
  { numSets :: Int
  }
  deriving (Eq, Ord, Show, Generic)

data Game = Game
  { layout :: Layout
  , foundation :: Foundation
  }
  deriving (Eq, Show, Generic)

data Move
  = MoveStack MoveStack
  | FlipCard FlipCard
  | MoveToFoundation MoveToFoundation
  deriving (Eq, Show, Read, Generic)

data MoveStack = MS
  { fromIndex :: Int
  , toIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

newtype FlipCard = FC
  { pileIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

newtype MoveToFoundation = MTF
  { pileIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

data InvalidMove
  = CardFlipOnUnexposedPile Int
  | CardFlipOnEmptyPile Int
  | IncompleteSet Int
  | MismatchingStacks Int Int
  | EmptyStackSource Int
  | MoveStackOntoFaceDownCards Int
  | SourceIsTarget Int
  deriving (Eq, Show, Read, Generic)

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
