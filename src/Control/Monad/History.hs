{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.History
  ( MonadHistory(..)
  , HistoryT
  , runHistoryT
  , historyHas
  )
  where

import Prelude

import Control.Monad.State.Strict
import Control.Monad.Reader

import Data.HashSet
import qualified Data.HashSet as Set
import Data.Hashable


type Set = HashSet
type ItemConstraint s = (Hashable s, Eq s)


-- A monad with capabilities that are more permissive than Writer, but less capable than State.
-- Allows viewing a set & appending items to it, but disallows deleting from it
class (ItemConstraint a, Monad m) => MonadHistory a m | m -> a where
  getHistory :: m (Set a)
  saveToHistory :: a -> m ()

historyHas :: MonadHistory a m => a -> m Bool
historyHas val = Set.member val <$> getHistory

newtype HistoryT s m a = HistoryT
  { toStateT :: StateT (Set s) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    )

runHistoryT :: (ItemConstraint s, Monad m) => HistoryT s m a -> m a
runHistoryT = flip evalStateT mempty . toStateT

instance (ItemConstraint s, Monad m) => MonadHistory s (HistoryT s m) where
  getHistory = HistoryT get
  saveToHistory = HistoryT . modify . insert

instance MonadReader r m => MonadReader r (HistoryT s m) where
  ask = lift ask
  local f = HistoryT . localState f .  toStateT
    where
      localState = mapStateT . local


instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadHistory a m
  )
  => MonadHistory a (t m) where
  getHistory = lift getHistory
  saveToHistory = lift . saveToHistory

