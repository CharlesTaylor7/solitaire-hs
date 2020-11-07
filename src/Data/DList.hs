{-# language FlexibleInstances #-}
module Data.DList
  ( DList
  , singleton
  , cons
  , intercalate
  ) where

import Prelude
import Data.Monoid
import GHC.Exts
import qualified Data.List as List


newtype DList a = DList ([a] -> [a])
  deriving (Semigroup, Monoid) via Endo [a]


intercalate :: DList a -> [DList a] -> DList a
intercalate sep = List.foldr (<>) mempty . List.intersperse sep

singleton :: a -> DList a
singleton a = cons a mempty

cons :: a -> DList a -> DList a
cons a (DList f) = DList $ (a :) . f


instance IsList (DList a) where
  type Item (DList a) = a

  toList :: DList a -> [a]
  toList (DList f) = f []

  fromList :: [a] -> DList a
  fromList as = DList $ (as <>)


instance Foldable DList where
  foldMap f = foldMap f . toList
