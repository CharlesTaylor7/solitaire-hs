module Examples.Boring where

import Solitaire.Boring


main :: IO ()
main = do
  config <- rightOrThrow $ mkConfig
    (NumSets 2)
    (Piles
      [ pileCounts 1 0
      , pileCounts 1 1
      , pileCounts 1 2
      , pileCounts 1 3
      ]
    )
  runGame @Boring config
