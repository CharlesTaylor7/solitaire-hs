{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.History
  (
    runHistory
  , runHistoryT
  , MonadHistory(..)
  )
  where

import Prelude ((.))
import Control.Monad.State.Strict

-- A monad with capabilities that are more permissive than Writer, but less capable than State.
-- Allows viewing history, but disallows deleting history
class Monad m => MonadHistory a m where
  history :: m [a]
  record :: a -> m ()

instance (Monad m, MonadState [a] m) => MonadHistory a m where
  history = get
  record a = modify (a : )

runHistory = runState
runHistoryT = runStateT
