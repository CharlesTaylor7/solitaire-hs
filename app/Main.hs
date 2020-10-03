{-# options_ghc -Wwarn #-}
module Main where

import Solitaire.Yukon


main :: IO ()
main = do
  config <- rightOrThrow $ defaultConfig

  let
    collectYukonStats = collectStats @Yukon config

  stats <- collectYukonStats StatsConfig
    { numRuns = 20
    , microSecondsTimeout = 5 * (10 ^ 6)
    }

  prettyPrint stats
