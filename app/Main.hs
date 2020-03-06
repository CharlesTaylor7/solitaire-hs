module Main where

import Solitaire

main :: IO ()
main = runGame env
  where
    env = Env
      { _env_numSets = 7
      , _env_numPiles = 7
      }
