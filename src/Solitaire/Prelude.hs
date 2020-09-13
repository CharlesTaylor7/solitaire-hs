{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Prelude (module X) where
import GHC.Generics (Generic)

-- custom
import Utils as X
import PrettyPrinter as X

import Control.Monad.History as X
import Control.Monad.PQueue as X

-- base
import Prelude as X hiding (print)

import Control.Arrow as X ((&&&), (|||), (>>>))
import Control.Exception as X
import Control.Monad.Zip as X (mzip)

import Data.Char as X (intToDigit)
import Data.Coerce as X (coerce)
import Data.Bifunctor as X
import Data.Foldable as X
import Data.Function as X (on)
import Data.List as X (intercalate, transpose, sortOn)
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)
import Data.Ord as X

import Data.String as X (IsString(..))
import GHC.Exts as X (IsList(fromList, Item))
import GHC.Generics as X (Generic)

-- text
import Data.Text as X (Text)

-- bytestring
import Data.ByteString as X (ByteString)

-- vector
import Data.Vector.Mutable as X (IOVector)
import Data.Vector as X (Vector)

-- containers
import Data.IntMap as X (IntMap)
import Data.Map as X (Map)
import Data.Set as X (Set)

-- hashable
import Data.Hashable as X

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
