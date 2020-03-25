module ListT where

import Data.Coerce
import RIO hiding (over)
import Pipes (ListT(..))
import qualified Pipes

import Control.Lens (over, _Just, _2)

list :: Monad m => [a] -> ListT m a
list = Select . Pipes.each

listT :: Monad m => m [a] -> ListT m a
listT xs = lift xs >>= list

runListT :: Monad m => Int -> ListT m a -> m [a]
runListT 0 _ = pure []
runListT n list = do
  maybe <- next list
  case maybe of
    Just (x, prod) ->
      (x : ) <$> runListT (n-1) prod
    Nothing ->
      pure []

runList :: Int -> ListT Identity a -> [a]
runList = (runIdentity .) . runListT

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a) = Nothing
eitherToMaybe (Right b) = Just b

next :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
next = fmap (over (_Just . _2) Select . eitherToMaybe) . Pipes.next . coerce
