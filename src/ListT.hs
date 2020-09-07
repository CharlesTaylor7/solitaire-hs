module ListT where

import Prelude

import Control.Monad.Trans (lift)

import Data.Coerce
import Data.Functor.Identity

import Pipes (ListT(..))
import qualified Pipes

import Control.Lens (over, _Just, _2, preview, _Right)


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

next :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
next = fmap (over (_Just . _2) Select . preview _Right) . Pipes.next . coerce
