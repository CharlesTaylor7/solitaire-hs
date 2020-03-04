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
shuffleIO :: Foldable f => f a -> IO [a]
shuffleIO coll = do
  let vector = V.fromList . toList $ coll
  thawed <- V.thaw vector
  shuffleIOVector thawed
  frozen <- V.freeze thawed
  pure $ toList frozen

shuffleIOVector :: IOVector a -> IO ()
shuffleIOVector vector =
  let
    n = MV.length vector
    indices = [0..(n-1)] :: [Int]
  in
    for_ indices $ \i ->
    randomRIO (0, i) >>=
    MV.swap vector i

newGame :: IO Game
newGame = do
  shuffled <- shuffleIO deck
  let piles = toPile <$> chunksOf initialPileSize shuffled
  let layout = Layout $ indexFrom 0 piles
  let foundation = Foundation 0
  pure $ Game layout foundation

gameLoop :: IO ()
gameLoop =
  let
    act game = do
      putStrLn $ pretty game
      getLine
      move <- randomIO
      pure $ moveReducer move game ^. swapped
  in do
    game <- newGame
    error <- loopM act game
    print error
    pure ()
