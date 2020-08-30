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

import Prelude hiding (splitAt)
import Control.Applicative (pure)
import Control.Monad.State.Strict

type PQueue = MinPQueue

-- A state monad wrapped around a priority queue
class (Ord k, Monad m) => MonadPQueue k v m | m -> k, m -> v where
  deleteMin :: m (Maybe (k, v))
  upsert :: k -> v -> m ()

instance (Ord k, Monad m) => MonadPQueue k v (StateT (PQueue k v) m) where
  deleteMin = do
    queue <- get
    case PQueue.minViewWithKey queue of
      Just ((k, v), queue) -> do
        put queue
        pure $ Just (k, v)
      Nothing -> pure Nothing

  upsert key value = do
    queue <- get
    let
      (ascPairs, queue) = splitAt key queue

      peeled = PQueue.fromAscList ascPairs
      unioned = PQueue.union peeled queue
      inserted = PQueue.insert key value unioned

    put inserted

splitAt :: Ord k => k -> PQueue k a -> ([(k, a)], PQueue k a)
splitAt key q =
  case PQueue.minViewWithKey q of
    Just (pair@(k, a), q') ->
      if key < k
      then
        let
          (pairs, q'') = splitAt k q'
        in
          (pair : pairs, q')
      else
        ([], q')
    Nothing ->
      ([], q)

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
