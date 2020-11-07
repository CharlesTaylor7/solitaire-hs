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
import LinearRegression

data CliArgs w = CliArgs
  { numTrials  :: w ::: Int <!> "10"  <?> "Number of trials to run"
  , limitGames :: w ::: Int <!> "100" <?> "Limit number of games to test"
  , step       :: w ::: Float <!> "0.1" <?> "Step size"
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

    test = testWeights optimizeConfig

  runTime <- test initialWeights

  prettyPrint runTime
  pure ()

data PartialDArgs = PartialDArgs
  { step :: Float
  , field :: Text
  , runTime :: RunTime
  , weights :: Weights
  , function :: Weights -> IO RunTime
  }
  deriving stock (Generic)

partialDerivative :: PartialDArgs -> IO RunTime
partialDerivative args = do
  let
    w_1 :: Weights
    w_1 = args ^. #weights & ix (args ^. #field) -~ (args ^. #step)

    w_2 :: Weights
    w_2 = args ^. #weights & ix (args ^. #field) +~ (args ^. #step)

    r0 = args ^. #runTime

  r1 <- args ^. #function $ w_1
  r2 <- args ^. #function $ w_2

  pure $ undefined

data Uncertain a = a :+- a

data Line = Line
  { m :: Uncertain Float
  , b :: Uncertain Float
  }
  deriving stock (Generic)


linearRegression :: [(Weight, RunTime)] -> Line
linearRegression = undefined



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


testWeights :: OptimizeConfig -> Weights -> IO RunTime
testWeights config weights = do
  let
    games = config ^. #games
    numTrials = config ^. #numTrials

  times <- for games $ \game -> do
    let
      run = runGame @Yukon game
        & (runReaderT ?? weights)
        & time_

    sequenceA $ replicate numTrials run

  let
    sortedData = join times & sort & V.fromList
    (mean, variance) = meanVariance sortedData
    standardDeviation = sqrt variance

  pure $ RunTime { mean, standardDeviation }


updateWeightsAtRandom
  :: forall k m. (Ord k, MonadRandom m)
  => Weights
  -> m Weights
updateWeightsAtRandom map = do
  let
    n = length map
    keys = (map ^.. ifolded . asIndex)

  delta <- randomDelta n

  pure $ flip execState map $
    ifor_ keys $ \i key -> do
      ix key += (delta ^?! ix i)
