{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
module Data.Union
  ( Union(..)
  , inject
  , (:<)
  ) where

import Prelude
import Control.Applicative
import Data.Void

import qualified Data.Sum as FastSum


newtype Union variants = Union (FastSum.Sum (Record variants) Void)

type Record variants = Map Const variants

type (:<) v vs = Const v FastSum.:< Record vs


inject :: v :< vs => v -> Union vs
inject = Union . FastSum.inject . Const


type family Map (f :: * -> * -> *) (xs :: [*]) where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs
