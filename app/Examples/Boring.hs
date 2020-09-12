module Examples.Boring where

import Solitaire.Boring


main :: IO ()
main = do
  config <- rightOrThrow $ mkConfig
    (NumSets 2)
    (Piles
      [ Pile 1 0
      , Pile 1 1
      , Pile 1 2
      , Pile 1 3
      ]
    )
  runGame @Boring config
