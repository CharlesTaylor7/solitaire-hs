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

-- Data types
data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum)

data Pile = Pile
  { faceUp :: Vector Card
  , faceDown :: Vector Card
  }
  deriving (Eq, Show, Read)

data Foundation = Foundation
  { numSets :: Int
  }
  deriving (Eq, Show, Read)

data Game = Game
  { layout :: IntMap Pile
  , foundation :: Foundation
  }
  deriving (Eq, Show, Read)

data Move
  = MoveStack MoveStack
  | MoveToFoundation MoveToFoundation
  | FlipCard FlipCard
  deriving (Eq, Show, Read)

data FlipCard = FC
  { fc_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

data MoveToFoundation = MTF
  { mtf_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

data MoveStack = MS
  { ms_pileIndex1 :: Int
  , ms_pileIndex2 :: Int
  }
  deriving (Eq, Show, Read)

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
  printLn solution
