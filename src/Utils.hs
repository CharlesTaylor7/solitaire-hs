{-# options_ghc -Wwarn #-}
module Utils
  ( rightOrThrow
  , enumerate
  , enumSize
  , maybeToError
  , distributed
  , Input(..)
  , userInput
  , print
  , printS
  , loopM
  , indexFrom
  , shuffle
  ) where

-- base
import Prelude hiding (print)
import qualified Prelude

import Control.Arrow
import Control.Exception

import Data.Foldable

import GHC.Exts (IsList(..))

-- lens
import Control.Lens

-- -- mtl
import Control.Monad.Except

-- MonadRandom
import Control.Monad.Random

-- vector
import Data.Vector.Mutable (IOVector)

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


rightOrThrow :: (MonadIO m, Exception e) => Either e a -> m a
rightOrThrow = liftIO . throwIO ||| pure

enumerate :: forall l a. (Bounded a, Enum a, IsList l, Item l ~ a) => l
enumerate = [minBound..maxBound]

enumSize :: forall a. (Bounded a, Enum a) => Int
enumSize = hi - lo + 1
  where
    hi = fromEnum (maxBound :: a)
    lo = fromEnum (minBound :: a)

loopM :: Monad m => (a -> ExceptT end m a) -> a -> m end
loopM act x = (runExceptT $ act x) >>= pure ||| loopM act

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError e Nothing = throwError e
maybeToError _ (Just a) = pure a

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

printS :: MonadIO m => String -> m ()
printS = liftIO . putStrLn

newtype Input = Input String
  deriving Eq

userInput :: (MonadIO m, Read a) => m (Either Input a)
userInput = safeRead <$> liftIO getLine

safeRead :: Read a => String -> Either Input a
safeRead s =
  case reads s of
    [(a, "")] -> Right a
    _ -> Left . Input $ s

indexFrom :: Int -> [a] -> IntMap a
indexFrom offset = M.fromAscList . zip [offset..]

shuffle :: MonadIO m => [a] -> m [a]
shuffle coll = do
  let vector = V.fromList coll
  thawed <- liftIO $ V.thaw vector
  liftIO $ shuffleIOVector thawed
  frozen <- liftIO $ V.freeze thawed
  pure $ V.toList frozen

shuffleIOVector :: IOVector a -> IO ()
shuffleIOVector vector =
  let
    n = MV.length vector
    indices = [0..(n-1)] :: [Int]
  in
    for_ indices $ \i ->
      getRandomR (0, i) >>=
      liftIO . MV.swap vector i

distributed :: Iso' (a, Either b c) (Either (a, b) (a, c))
distributed = iso to from
  where
    to (a, Left b) = Left (a, b)
    to (a, Right c) = Right (a, c)
    from (Left (a, b)) = (a, Left b)
    from (Right (a, c)) = (a, Right c)
