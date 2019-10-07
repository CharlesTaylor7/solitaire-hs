{-# LANGUAGE ScopedTypeVariables #-}

module Solitaire.Utils where

import Control.Arrow ((|||))

enumSize :: forall a. (Bounded a, Enum a) => a -> Int
enumSize _ = hi - lo + 1
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
