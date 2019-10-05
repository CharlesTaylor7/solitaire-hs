{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}
{-# LANGUAGE RankNTypes #-}
module Solitaire where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV

import Data.Foldable
import Data.Traversable
import Control.Applicative

import Solitaire.Types

-- Pure declarations
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  if null xs
  then []
  else
    let (chunk, rest) = splitAt n xs
    in chunk : chunksOf n rest

deck :: [Card]
deck = enumFromTo One Five >>= replicate 3

toPile :: [Card] -> Pile
toPile cards =
  let (faceUp, faceDown) = splitAt 2 cards
  in Pile (V.fromList faceUp) (V.fromList faceDown)

indexFrom :: Int -> [a] -> IntMap a
indexFrom offset = M.fromAscList . zip [offset..]

solve :: Game -> [(Move, Game)]
solve = _

moveReducer :: Game -> Move -> Game
moveReducer game move =
  case move of
    MoveStack (MS (i j)) -> _
    FlipCard (FC i) -> _
    MoveToFoundation (MTF i) -> _

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
  in
    for_ [1..n] $ \i ->
      for_ [1..i] $ \j ->
        MV.swap vector i j

newGame :: IO Game
newGame = do
  shuffled <- shuffleIO deck
  let piles = toPile <$> chunksOf 5 shuffled
  let layout = indexFrom 0 piles
  let foundation = Foundation 0
  pure $ Game layout foundation

main :: IO ()
main = do
  game <- newGame
  let solution = solve game
  print solution
