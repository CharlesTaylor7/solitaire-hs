{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}
{-# LANGUAGE TypeApplications #-}
module Solitaire
  ( module Solitaire
  , module Solitaire.Utils
  , module Solitaire.Types
  , module M
  , module V
  , module MV
  ) where

-- base
import Data.Foldable
import Data.Traversable
import Control.Applicative

-- libraries
import Control.Lens

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV

import System.Random

-- app
import Solitaire.PrettyPrinter
import Solitaire.Types
import Solitaire.Utils

-- Pure declarations

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
numCards = enumSize @Card undefined

initialPileSize :: Int
initialPileSize = length deck `div` numPiles

moveReducer :: Move -> Game -> Game
moveReducer move =
  case normalize 3 move of
    MoveStack (MS i j) ->
      let
        moveStack = id
      in layout . _Layout %~ moveStack

    FlipCard (FC i) -> undefined
    MoveToFoundation (MTF i) -> undefined

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
    for_ [0..(n-1)] $ \i ->
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
      let g' = moveReducer move game
      pure $ Left $ g'
  in do
    game <- newGame
    solution <- loopM act game
    pure ()
