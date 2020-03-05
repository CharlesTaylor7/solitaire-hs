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

runGame :: IO ()
runGame = do
  game <- newGame
  error <- loopM act game
  putStrLn "done"

act :: Game -> IO (Either Game ())
act game = do
  prettyPrint game
  getLine
  let steps = validSteps game
  putStrLn "Valid moves: "
  prettyPrint $ map _step_move steps
  next <- randomElem steps
  case next of
    Nothing -> do
      putStrLn "No valid moves"
      pure $ Right ()
    Just (Step move game) -> do
      prettyPrint move
      pure $ Left game

newGame :: IO Game
newGame = do
  shuffled <- shuffleIO deck
  let piles = toPile <$> chunksOf initialPileSize shuffled
  let layout = Layout $ indexFrom 0 piles
  let foundation = Foundation 0
  pure $ Game layout foundation

solve :: Game -> [Step]
solve = undefined

deck :: [Card]
deck = enumFromTo One Five >>= replicate 3

toPile :: [Card] -> Pile
toPile cards =
  let (faceUp, faceDown) = splitAt 2 cards
  in Pile (V.fromList faceUp) (V.fromList faceDown)

indexFrom :: Int -> [a] -> IntMap a
indexFrom offset = M.fromAscList . zip [offset..]

numCardCopies :: Int
numCardCopies = 3

numCards :: Int
numCards = enumSize @Card

initialPileSize :: Int
initialPileSize = length deck `div` numPiles
