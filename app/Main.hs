module Main where

import Solitaire

main :: IO ()
main = runGame env
  where
    env = Env
      { _env_numSets = 3
      , _env_numPiles = 4
      , _env_numFaceUpPerPile = 1
      }
