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

ifThenError :: (MonadError e m) => Bool -> e -> m ()
ifThenError True  e = throwError e
ifThenError False _ = pure ()

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError e Nothing = throwError e
maybeToError _ (Just a) = pure a

shuffleIO :: (MonadIO m, MonadRandom m, Foldable f) => f a -> m [a]
shuffleIO coll = do
  let vector = V.fromList . toList $ coll
  thawed <- liftIO $ V.thaw vector
  shuffleIOVector thawed
  frozen <- liftIO $ V.freeze thawed
  pure $ toList frozen

shuffleIOVector :: (MonadIO m, MonadRandom m) => IOVector a -> m ()
shuffleIOVector vector =
  let
    n = MV.length vector
    indices = [0..(n-1)] :: [Int]
  in
    for_ indices $ \i ->
      getRandomR (0, i) >>=
      liftIO . MV.swap vector i
