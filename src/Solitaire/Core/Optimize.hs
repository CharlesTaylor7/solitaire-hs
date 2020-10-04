module Solitaire.Core.Optimize where

import Solitaire.Prelude

import Solitaire.Core.Engine
import Solitaire.Core.Rules
import Solitaire.Core.Config
import Solitaire.Core.Utils

import Solitaire.Core.Move.Class
import qualified Solitaire.Core.Move.Class as Move

import qualified Data.HashSet as Set

import System.Timeout
import GenericUtils (constructorName)

import qualified Data.Map as Map


data GameLabel = Solvable | Unsolvable | TimedOut
  deriving stock (Ord, Eq, Generic, Show)
  deriving anyclass (Pretty)


-- | select a random point inside the unit n-dimensional ball
randomDelta :: MonadRandom m => Int -> m [Float]
randomDelta n = do
  vars <- sequence $ replicate n $ getRandomR (0, 1)
  -- nth root
  r <- (** (1 / fromIntegral n)) <$> getRandomR (0, 1)
  let
    norm = vars & sumOf (folded . to (^ 2)) & sqrt
    scalar = r / norm

  pure $ (scalar *) <$> vars


runTrials :: forall rs. Solitaire rs => AppConfig -> IO (Map GameLabel [Game rs])
runTrials config = do
  conclusions <-
    for ([1 .. config ^. #stats . #numTrials] :: [Int]) $ \i -> do
      putStrLn $ "Run #" <> show i

      game <- runReaderT ?? config $ magnify #game $
        newGame @rs

      label <- runGame @rs game
        & magnify (#stats . #heuristicWeights)
        & (runReaderT ?? config)
        & timeout (config ^. #stats . #microSecondsTimeout)
        <&> toGameLabel

      pure (label, game)

  pure $ conclusions & toMap


toGameLabel :: Maybe (GameSolve game) -> GameLabel
toGameLabel (Just (Solution _)) = Solvable
toGameLabel (Just NoSolution) = Unsolvable
toGameLabel Nothing = TimedOut
