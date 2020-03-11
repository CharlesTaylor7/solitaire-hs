module Main where

import Solitaire
import qualified Data.IntMap as M

main :: IO ()
main = runGame env
  where
    Just env = mkEnv
      (NumSets 2)
      (Piles
        [ pileCounts 1 0
        , pileCounts 1 1
        , pileCounts 1 2
        , pileCounts 1 3
        ]
      )
