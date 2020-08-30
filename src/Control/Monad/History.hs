{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.History
  ( runHistory
  , runHistoryT
  , MonadHistory(..)
  )
  where

import Prelude (Ord, (.))
import Control.Monad.State.Strict
import Data.Set

-- A monad with capabilities that are more permissive than Writer, but less capable than State.
-- Allows viewing a set & appending items to it, but disallows deleting from it
class Monad m => MonadHistory a m | m -> a where
  getHistory :: m (Set a)
  saveToHistory :: a -> m ()

instance (Ord a, Monad m) => MonadHistory a (StateT (Set a) m) where
  getHistory = get
  saveToHistory x = modify (insert x)

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadHistory a m
  )
  => MonadHistory a (t m) where
  getHistory = lift getHistory
  saveToHistory = lift . saveToHistory

runHistory :: State s a -> s -> (a, s)
runHistory = runState

runHistoryT :: StateT s m a -> s -> m (a, s)
runHistoryT = runStateT
