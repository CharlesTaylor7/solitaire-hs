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
  print error
  pure ()

act :: Game -> IO (Either Game InvalidMove)
act game = do
  prettyPrint game
  getLine
  move <- getRandom
  let either = moveReducer move game
  pure $ either ^. swapped

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
