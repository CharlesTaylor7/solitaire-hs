{-# options_ghc -Wwarn #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
module Main where

import Solitaire.Yukon

import System.Directory
import System.Environment
import System.Microtimer

import qualified Data.Vector as V

import Statistics.Sample
import Data.List (sort)
import Options.Generic


data CliArgs w = CliArgs
  { numTrials  :: w ::: Int <!> "10"  <?> "Number of trials to run"
  , limitGames :: w ::: Int <!> "100" <?> "Limit number of games to test"
  }
  deriving stock (Generic)

deriving instance Show (CliArgs Unwrapped)
instance ParseRecord (CliArgs Wrapped)


main :: IO ()
main = do
  args :: CliArgs Unwrapped <- unwrapRecord "Optimize heuristics"

  let
    limitGames = args ^. #limitGames
    numTrials = args ^. #numTrials

  fileNames <- take limitGames <$> listDirectory "games/Solvable"

  games <- for fileNames $ \name -> do
    fileContents <- readFile $ "games/Solvable/" <> name
    pure $ read fileContents

  let
    optimizeConfig = OptimizeConfig { numTrials, games }

    initialWeights = mempty
      & at "numFaceUp" ?~ 2
      & at "numFaceDown" ?~ 5
      & at "totalRunScore" ?~ (-0.5)

  runTime <- testWeights initialWeights
    & (runReaderT ?? optimizeConfig)

  prettyPrint runTime
  pure ()


data OptimizeConfig = OptimizeConfig
  { numTrials :: !Int
  , games :: ![Game]
  }
  deriving stock (Generic)


data RunTime = RunTime
  { mean :: !Double
  , standardDeviation :: !Double
  }
  deriving stock (Generic)


instance Pretty RunTime where
  prettyExpr run =
    prettyExpr $ (Empty :: Map Text Text)
      & at "mean" ?~ (run ^. #mean . to formatSeconds . packed)
      & at "std dev" ?~ (run ^. #standardDeviation . to formatSeconds . packed)


testWeights :: (MonadReader OptimizeConfig m, MonadIO m) => Weights -> m RunTime
testWeights weights = do
  games <- view #games
  numTrials <- view #numTrials

  times <- for games $ \game -> do
    let
      run = runGame @Yukon game
        & (runReaderT ?? weights)
        & time_
        & liftIO

    sequenceA $ replicate numTrials run

  let
    sortedData = join times & sort & V.fromList
    (mean, variance) = meanVariance sortedData
    standardDeviation = sqrt variance

  pure $ RunTime { mean, standardDeviation }


updateWeightsAtRandom
  :: forall k m. (Ord k, MonadRandom m)
  => Map k Float
  -> m (Map k Float)
updateWeightsAtRandom map = do
  let
    n = length map
    keys = (map ^.. ifolded . asIndex)

  delta <- randomDelta n

  pure $ flip execState map $
    ifor_ keys $ \i key -> do
      ix key += (delta ^?! ix i)
