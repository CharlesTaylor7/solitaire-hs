module Main where

import Solitaire

import qualified Solitaire.Boring.Types as Boring


main :: IO ()
main = do
  config <- rightOrThrow $ mkConfig
    (NumSets 2)
    (Piles
      [ Boring.pileCounts 1 0
      , Boring.pileCounts 1 1
      , Boring.pileCounts 1 2
      , Boring.pileCounts 1 3
      ]
    )
  runGame @Boring config
