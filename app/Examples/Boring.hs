module Examples.Boring where

import Solitaire.Boring


main :: IO ()
main = void $ runGame @Boring staggeredConfig
