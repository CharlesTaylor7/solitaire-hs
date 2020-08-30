{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# options_ghc -Wwarn #-}
module Control.Monad.PQueue
  ( MonadPQueue(..)
  , PQueue
  )
  where

import qualified Data.PQueue.Prio.Min as PQueue
import Data.PQueue.Prio.Min (MinPQueue)

import Prelude (Ord, Maybe, (.))
import Control.Monad.State.Strict

type PQueue = MinPQueue

-- A state monad wrapped around a priority queue
class (Ord k, Monad m) => MonadPQueue k v m | m -> k, m -> v where
  deleteMin :: m (Maybe (k, v))
  upsert :: k -> v -> m ()

instance (Ord k, Monad m) => MonadPQueue k v (StateT (PQueue k v) m) where
  -- deleteMin =  _deleteMin
  -- upsert = _upsert

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadPQueue k v m
  )
  => MonadPQueue k v (t m) where
  deleteMin = lift deleteMin
  upsert = (lift .) . upsert

runPQueue :: State s a -> s -> (a, s)
runPQueue = runState

runPQueueT :: StateT s m a -> s -> m (a, s)
runPQueueT = runStateT
