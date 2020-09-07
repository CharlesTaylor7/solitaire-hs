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

import Control.Monad.State.Strict
import Control.Monad.Reader


-- A state monad wrapped around a priority queue
class (Ord k, Monad m) => MonadPQueue k v m | m -> k, m -> v where
  queuePopMin :: m (Maybe (k, v))
  queueInsert :: k -> v -> m ()

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

-- run the computation starting with an empty queue
runPQueueT :: (Ord k, Monad m) => PQueueT k v m a -> m a
runPQueueT = flip evalStateT mempty . toStateT

instance (Ord k, Monad m) => MonadPQueue k v (PQueueT k v m) where
  queuePopMin = PQueueT $ do
    queue <- get
    case Pri.minViewWithKey queue of
      Just ((k, v), queue) -> do
        put queue
        pure $ Just (k, v)
      Nothing ->
        pure Nothing

  queueInsert key value = PQueueT . modify $ Pri.insert key value

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
  queuePopMin = lift queuePopMin
  queueInsert = (lift .) . queueInsert
