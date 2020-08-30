{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# options_ghc -Wwarn #-}
module Control.Monad.RandomInstances where

import Prelude
import Control.Monad.Random

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadRandom m
  )
  => MonadRandom (t m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

