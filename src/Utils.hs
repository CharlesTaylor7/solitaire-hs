module Utils where

import Prelude (putStrLn, maximum, enumFromTo, getLine, print)
import Data.Monoid
import Data.List (intercalate, transpose, splitAt)
import Control.Arrow
import Control.Monad.Zip (mzip)

-- rio
import RIO hiding (Lens, Lens', Getting, ASetter, ASetter', lens, (^.), to, view, over, set, sets)

-- lens
import Control.Lens

-- -- mtl
import Control.Monad.Except
import Control.Monad.State.Strict

-- MonadRandom
import Control.Monad.Random

-- vector
import Data.Vector.Mutable (IOVector)

import qualified Data.Foldable as Foldable
import Data.List ((!!))
import GHC.Exts (IsList(..))

import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

enumerate :: (Bounded a, Enum a, IsList l, Item l ~ a) => l
enumerate = [minBound..maxBound]

enumSize :: forall a. (Bounded a, Enum a) => Int
enumSize = hi - lo + 1
  where
    hi = fromEnum (maxBound :: a)
    lo = fromEnum (minBound :: a)

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = act x >>= loopM act ||| pure

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0 = error "Cannot take chunks of 0 or negative numbers"
  | otherwise = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n xs

ifThenError :: (MonadError e m) => Bool -> e -> m ()
ifThenError True  e = throwError e
ifThenError False _ = pure ()

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError e Nothing = throwError e
maybeToError _ (Just a) = pure a

randomElem
  :: (MonadRandom m)
  => [a]
  -> m (Maybe a)
randomElem [] = pure Nothing
randomElem as = Just . (as !!) <$> getRandomR (0, n-1)
  where n = length as