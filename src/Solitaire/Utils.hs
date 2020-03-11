module Solitaire.Utils where

import Solitaire.Imports
import qualified Data.Vector as V

cards :: PileCards -> Vector Card
cards pile = up <> down
  where
    up = pile ^. faceUp
    down = pile ^. faceDown

emptyPile :: PileCards
emptyPile = Pile V.empty V.empty

getDeck :: MonadReader Config m => m [Card]
getDeck = do
  numSets <- view config_numSets
  pure $ enumerate >>= replicate numSets

pileCountsSize :: PileCounts -> Int
pileCountsSize counts =
  counts ^. faceUp +
  counts ^. faceDown

toPile :: [Card] -> PileCounts -> PileCards
toPile cards counts =
  let
    n = counts ^. faceUp
    (up, down) = splitAt n cards
  in
    Pile (V.fromList up) (V.fromList down)

list :: Monad m => [a] -> ListT m a
list = Select . each

listT :: Monad m => m [a] -> ListT m a
listT xs = lift xs >>= list

runListT :: Monad m => Int -> ListT m a -> m [a]
runListT 0 _ = pure []
runListT n (Select producer) = do
  either <- next producer
  case either of
    Left _ -> pure []
    Right (x, prod) -> (x : ) <$> runListT (n-1) (Select prod)

runList :: Int -> ListT Identity a -> [a]
runList = (runIdentity .) . runListT
