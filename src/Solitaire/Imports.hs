{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Imports (module X) where

import Solitaire.Types as X hiding (pattern Config)

-- custom
import ListT as X
import Utils as X

import Control.Monad.History as X
import Control.Monad.PQueue as X

-- base
import Prelude as X hiding (print)

import Control.Arrow as X ((&&&), (|||), (>>>))
import Control.Exception as X
import Control.Monad.Zip as X (mzip)

import Data.Ord as X
import Data.Bifunctor as X
import Data.Foldable as X hiding (find)
import Data.List as X (intercalate, transpose, sortOn)
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)

-- text
import Data.Text as X (Text)

-- pipes
import Pipes as X (ListT(Select), Producer, Pipe, yield, each, await, hoist)

-- lens
import Control.Lens as X hiding (each, (<|))
import Control.Lens.Extras as X (is)

-- generic-lens
import Data.Generics.Labels ()

-- mtl
import Control.Monad.Except as X
import Control.Monad.State.Strict as X
import Control.Monad.Writer.Strict as X
import Control.Monad.Reader as X

-- transformers
import Control.Monad.Trans.Maybe as X

-- MonadRandom
import Control.Monad.Random as X hiding (fromList, next)

-- vector
import Data.Vector.Mutable as X (IOVector)
import Data.Vector as X (Vector)

-- containers
import Data.Set as X (Set)
import Data.Map as X (Map)
