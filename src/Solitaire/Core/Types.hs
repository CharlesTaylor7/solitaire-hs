{-# options_ghc -Wno-orphans #-}
module Solitaire.Core.Types where

import Solitaire.Prelude


data Config = Config
  { numSets :: Int
  , piles :: IntMap (Pile Int)
  }
  deriving (Eq, Show, Generic)


newtype Score = Score Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving (Eq, Show, Generic)

instance Enum Card where
  toEnum number =
    let (rank, suit) = number `quotRem` enumSize @Suit
    in Card (rank ^. enum) (suit ^. enum)

  fromEnum card =
    let
      rank = card ^. #rank . from enum
      suit = card ^. #suit . from enum
    in
      rank * (enumSize @Suit) + suit

instance Bounded Card where
  minBound = Card minBound minBound
  maxBound = Card maxBound maxBound

data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


data Suit
 = Diamonds
 | Clubs
 | Hearts
 | Spades
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


data Color
 = Red
 | Black
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


data Pile a = Pile
  { faceUp :: a
  , faceDown :: a
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- orphan instance for Hashable Intmap & Hashable Vector
-- TODO: Use newtype wrappers?
newtype SomeIndexedFoldable f a i = SomeIndexedFoldable (f a)
  deriving (Generic)

instance (FoldableWithIndex i f, Hashable i, Hashable a) => Hashable (SomeIndexedFoldable f a i) where
  hashWithSalt salt as =
    as ^.. #_SomeIndexedFoldable . ifolded . withIndex
    & hashWithSalt salt

deriving via (SomeIndexedFoldable IntMap a Int) instance Hashable a => Hashable (IntMap a)
deriving via (SomeIndexedFoldable Vector a Int) instance Hashable a => Hashable (Vector a)


-- pretty instances
deriving via WrappedShow Score instance Pretty Score
