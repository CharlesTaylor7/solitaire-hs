module Main where

import Solitaire

main :: IO ()
main = runGame env
  where
    env = Env
      { _env_numSets = 1
      , _env_numPiles = 4
      }
