{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}

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

-- Pure functions
solve :: Game -> [Move]
solve = _

-- Effectful
shuffleIO :: (Traversable t) => t a -> IO (t a)
shuffleIO = _

newGame :: IO Game
newGame = _

main :: IO ()
main = do
  game <- newGame
  let solution = solve game
  print solution
