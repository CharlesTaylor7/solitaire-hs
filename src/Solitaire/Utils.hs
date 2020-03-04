module Solitaire.Utils where

import Control.Arrow ((|||))
import GHC.Exts

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
chunksOf n xs =
  if null xs
  then []
  else
    let (chunk, rest) = splitAt n xs
    in chunk : chunksOf n rest

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight _ (Just b) = Right b
maybeToRight a _        = Left a
