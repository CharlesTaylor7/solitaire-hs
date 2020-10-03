{-# options_ghc -Wwarn #-}
module Main where

import Solitaire.Yukon


main :: IO ()
main = do
  gameConfig <- rightOrThrow $ defaultConfig

  let
    statsConfig = StatsConfig
      { numTrials = 100
      , microSecondsTimeout = 5 * (10 ^ 6)
      , heuristicWeights = mempty
        & at "numFaceUp" ?~ 2
        & at "numFaceDown" ?~ 5
        & at "totalRunScore" ?~ (-0.5)
      }
    appConfig = AppConfig statsConfig gameConfig

  games <- runGames @Yukon appConfig
  prettyPrint games


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
