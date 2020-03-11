module Main where

import Solitaire
import qualified Data.IntMap as M

main :: IO ()
main = do
  config <- fromEither $ mkConfig
    (NumSets 2)
    (Piles
      [ pileCounts 1 0
      , pileCounts 1 1
      , pileCounts 1 2
      , pileCounts 1 3
      ]
    )
  runGame config
