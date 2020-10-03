module Examples.Yukon where

import Solitaire.Yukon


main :: IO ()
main = do

  config <- rightOrThrow $ defaultConfig

  void $ runGame @Yukon config
