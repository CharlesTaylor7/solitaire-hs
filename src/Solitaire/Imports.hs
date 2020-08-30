{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Imports (module X) where

import Solitaire.Internal.Types as X hiding (pattern Config)

import ListT as X
import Utils as X
import Prelude as X (putStrLn, maximum, enumFromTo, getLine, read, repeat, zipWith)

import Data.Ord as X
import Data.Bifunctor as X
import Data.List as X (intercalate, transpose, splitAt, sortOn)
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)

import Control.Arrow as X ((&&&), (|||))
import Control.Monad.Zip as X (mzip)
import Control.Monad.History as X
import Control.Monad.PQueue as X
import Control.Monad.RandomInstances ()

-- rio
import RIO as X hiding (Lens, Lens', Getting, ASetter, ASetter', lens, (^.), to, view, over, set, sets, first, second)

-- pipes
import Pipes as X (ListT(Select), Producer, Pipe, yield, each, await, hoist)

-- lens
import Control.Lens as X hiding (each, (<|))

-- mtl
import Control.Monad.Except as X
import Control.Monad.State.Strict as X
import Control.Monad.Writer.Strict as X

-- transformers
import Control.Monad.Trans.Maybe as X

-- MonadRandom
import Control.Monad.Random as X hiding (fromList, next)

-- vector
import Data.Vector.Mutable as X (IOVector)
