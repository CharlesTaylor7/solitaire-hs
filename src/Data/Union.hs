{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
module Data.Union
  ( OpenSum
  , (:<)
  , inject
  ) where

import Prelude
import Control.Applicative
import Data.Void

import qualified Data.Sum as FastSum


type OpenSum vs = FastSum.Sum (Mapped vs) Void

type (:<) v vs = FastSum.Element (Const v) (Mapped vs)

inject :: v :< vs => v -> OpenSum vs
inject = FastSum.inject . Const


type family Map (f :: * -> * -> *) (xs :: [*]) where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type Mapped variants = Map Const variants
