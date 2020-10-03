{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module GenericUtils
  ( constructorName
  ) where

import Prelude
import GHC.Generics
import Data.Kind


constructorName :: (HasConstructor (Rep a), Generic a) => a -> String
constructorName = genericConstructorName . from

class HasConstructor (f :: Type -> Type) where
  genericConstructorName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstructorName (M1 x) = genericConstructorName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstructorName (L1 l) = genericConstructorName l
  genericConstructorName (R1 r) = genericConstructorName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstructorName x = conName x
