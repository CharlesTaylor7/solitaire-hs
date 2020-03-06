module Main where

import Solitaire
import qualified Data.IntMap as M

main :: IO ()
main = runGame env
  where
    env = Env
      { _env_numSets = 2
      , _env_piles = M.fromAscList . zip [0..] $
          [ pileCounts 1 0
          , pileCounts 1 1
          , pileCounts 1 2
          , pileCounts 1 3
          ]
      }
