{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Imports
  ( module Solitaire.Internal.Types
  , module Prelude
  , module RIO
  , module Pipes
  , module Control.Arrow
  , module Control.Lens
  , module Control.Monad.Except
  , module Control.Monad.Random
  , module Control.Monad.Cache
  , module Control.Monad.State.Strict
  , module Control.Monad.Writer.Strict
  , module Control.Monad.Zip
  , module Control.Monad.Trans.Maybe
  , module Data.Vector.Mutable
  , module Data.Bifunctor
  , module Data.Monoid
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Ord
  , module ListT
  , module Utils
  ) where

import Solitaire.Internal.Types hiding (pattern Config)

import ListT
import Utils
import Prelude (putStrLn, maximum, enumFromTo, getLine, read, repeat, zipWith)
import Data.Monoid
import Data.Ord
import Data.Bifunctor
import Data.List (intercalate, transpose, splitAt, sortOn)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Control.Arrow ((&&&), (|||))
import Control.Monad.Zip (mzip)
import Control.Monad.Cache

-- rio
import RIO hiding (Lens, Lens', Getting, ASetter, ASetter', lens, (^.), to, view, over, set, sets, first, second)

-- pipes
import Pipes (ListT(Select), Producer, Pipe, yield, each, await, hoist)

-- lens
import Control.Lens hiding (each)

-- mtl
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

-- transformers
import Control.Monad.Trans.Maybe

-- MonadRandom
import Control.Monad.Random hiding (fromList, next)

-- vector
import Data.Vector.Mutable (IOVector)

-- Some Orphans
-- instance MonadRandom m => MonadRandom (ListT m) where
--   getRandomR = lift . getRandomR
--   getRandom = lift getRandom
--   getRandomRs = lift . getRandomRs
--   getRandoms = lift getRandoms
