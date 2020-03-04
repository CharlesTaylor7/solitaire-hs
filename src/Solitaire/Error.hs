{--
  Error is structurally identical to Either but with different semantics.
  Either is error correcting in its semigroup instance.
  Error propagates errors in its semigroup instance.
--}
module Solitaire.Error where

import Solitaire.Imports

data Error e a
  = Error e
  | Success a

instance Semigroup a => Semigroup (Error e a) where
  Error e <> _ = Error e
  _ <> Error e = Error e
  Success x <> Success y = Success (x <> y)

instance Monoid a => Monoid (Error e a) where
  mempty = Success mempty
