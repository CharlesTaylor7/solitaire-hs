module Solitaire.Utils where

import Solitaire.Imports
import GHC.Exts (IsList(..))

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
