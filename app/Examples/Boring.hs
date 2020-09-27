module Examples.Boring where

import Solitaire.Boring


main :: IO ()
main = do
  runGame @Boring staggeredConfig
