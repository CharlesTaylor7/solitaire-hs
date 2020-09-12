{-# options_ghc -Wno-orphans #-}
module Solitaire.Core.Types
  ( Score(..)
  , Pile(..)
  ) where

import Solitaire.Prelude


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num, Generic)


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
