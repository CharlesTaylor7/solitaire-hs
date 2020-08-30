{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# options_ghc -Wwarn #-}
module Control.Monad.PQueue
  ( MonadPQueue(..)
  , PQueueT
  , runPQueueT
  )
  where

import qualified Data.PQueue.Prio.Min as Pri
import Data.PQueue.Prio.Min (MinPQueue)

import Prelude hiding (splitAt)

import Control.Applicative (pure)
import Control.Monad.State.Strict
import Control.Monad.Reader


-- A state monad wrapped around a priority queue
class (Ord k, Monad m) => MonadPQueue k v m | m -> k, m -> v where
  deleteMin :: m (Maybe (k, v))
  insert :: k -> v -> m ()

newtype PQueueT k v m a = PQueueT
  { toStateT :: StateT (MinPQueue k v) m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

-- state methods not to be exported
putQueue :: Monad m => MinPQueue k v -> PQueueT k v m ()
putQueue = PQueueT . put

getQueue :: Monad m => PQueueT k v m (MinPQueue k v)
getQueue = PQueueT get

modifyQueue :: Monad m => (MinPQueue k v -> MinPQueue k v) -> PQueueT k v m ()
modifyQueue = PQueueT . modify

-- run the computation starting with an empty queue
runPQueueT :: (Ord k, Monad m) => PQueueT k v m a -> m a
runPQueueT = flip evalStateT mempty . toStateT

instance (Ord k, Monad m) => MonadPQueue k v (PQueueT k v m) where
  deleteMin = do
    queue <- getQueue
    case Pri.minViewWithKey queue of
      Just ((k, v), queue) -> do
        putQueue queue
        pure $ Just (k, v)
      Nothing ->
        pure Nothing

  insert key value = modifyQueue $ Pri.insert key value

instance MonadReader r m => MonadReader r (PQueueT k v m) where
  ask = lift ask
  local f = PQueueT . localState f .  toStateT
    where
      localState = mapStateT . local

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadPQueue k v m
  )
  => MonadPQueue k v (t m) where
  deleteMin = lift deleteMin
  insert = (lift .) . insert
