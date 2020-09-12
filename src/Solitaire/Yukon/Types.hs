{-# options_ghc -Wno-orphans #-}
module Solitaire.Yukon.Types where

import Solitaire.Prelude


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

data Config = Config
  { numSets :: Int
  , piles :: IntMap PileCounts
  }
  deriving (Eq, Show, Read, Generic)

data Pile a = Pile
  { faceUp :: a
  , faceDown :: a
  }
  deriving (Eq, Ord, Show, Read, Generic)

type PileCounts = Pile Int
type PileCards = Pile (Vector Card)

pileCounts :: Int -> Int -> PileCounts
pileCounts = Pile

pileCards :: Vector Card -> Vector Card -> PileCards
pileCards = Pile

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Ord, Show, Read, Generic)


data Foundation = Foundation
  { numSets :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Game = Game
  { layout :: Layout
  , foundation :: Foundation
  }
  deriving (Eq, Ord, Show, Read, Generic)

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

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num, Generic)

-- hashable instances
instance Hashable Game
instance Hashable Layout
instance Hashable Foundation
instance Hashable Card
instance Hashable a => Hashable (Pile a)

newtype SomeFoldable f a i = SomeFoldable { getFoldable :: f a }

instance (FoldableWithIndex i f, Hashable a) => Hashable (SomeFoldable f a i) where
  hashWithSalt salt = hashWithSalt salt . toList . getFoldable

-- orphan instance for Hashable Intmap & Hashable Vector
-- TODO: Use newtype wrappers?
deriving via (SomeFoldable IntMap a Int) instance Hashable a => Hashable (IntMap a)
deriving via (SomeFoldable Vector a Int) instance Hashable a => Hashable (Vector a)


flipCard :: Int -> Move
flipCard = FlipCard . FC

moveToFoundation :: Int -> Move
moveToFoundation = MoveToFoundation . MTF

moveStack :: Int -> Int -> Move
moveStack = (MoveStack .) . MS
