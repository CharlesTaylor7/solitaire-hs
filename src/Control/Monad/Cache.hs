{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Cache
  ( runCache
  , runCacheT
  , MonadCache(..)
  )
  where

import Prelude (Ord)
import Control.Monad.State.Strict
import Data.Set

-- A monad with capabilities that are more permissive than Writer, but less capable than State.
-- Allows viewing a cache & inserting to it, but disallows deleting from it
class Monad m => MonadCache a m | m -> a where
  getCache :: m (Set a)
  saveToCache :: a -> m ()

instance (Ord a, Monad m, MonadState (Set a) m) => MonadCache a m where
  getCache = get
  saveToCache x = modify (insert x)

runCache :: State s a -> s -> (a, s)
runCache = runState

runCacheT :: StateT s m a -> s -> m (a, s)
runCacheT = runStateT
