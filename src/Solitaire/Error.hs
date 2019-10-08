module Solitaire.Error where

data Error e a
  = Error e
  | Success a

instance Semigroup a => Semigroup (Error e a) where
  Error e <> _ = Error e
  _ <> Error e = Error e
  Success x <> Success y = Success (x <> y)

instance Monoid a => Monoid (Error e a) where
  mempty = Success mempty
