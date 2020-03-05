{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}
module Solitaire
  ( module Solitaire
  , module Solitaire.Exports
  ) where

import Solitaire.Exports
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

deck :: [Card]
deck = enumFromTo One Five >>= replicate 3

toPile :: [Card] -> Pile
toPile cards =
  let (faceUp, faceDown) = splitAt 2 cards
  in Pile (V.fromList faceUp) (V.fromList faceDown)

indexFrom :: Int -> [a] -> IntMap a
indexFrom offset = M.fromAscList . zip [offset..]

data Step = Step
  { step_move :: Move
  , step_game :: Game
  }
  deriving (Eq, Read, Show)

solve :: Game -> [Step]
solve = undefined

numPiles :: Int
numPiles = 3

numCardCopies :: Int
numCardCopies = 3

numCards :: Int
numCards = enumSize @Card

initialPileSize :: Int
initialPileSize = length deck `div` numPiles

-- effectful
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

newGame :: IO Game
newGame = do
  shuffled <- shuffleIO deck
  let piles = toPile <$> chunksOf initialPileSize shuffled
  let layout = Layout $ indexFrom 0 piles
  let foundation = Foundation 0
  pure $ Game layout foundation

act :: Game -> IO (Either Game InvalidMove)
act game = do
  printP game
  liftIO getLine
  move <- getRandom
  let appResult = moveReducer move game
  pure $ appResult ^. swapped

gameLoop :: IO ()
gameLoop = do
  game <- newGame
  error <- loopM act game
  print error
  pure ()
