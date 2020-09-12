module Examples.Yukon where

import Solitaire.Yukon


main :: IO ()
main = do
  config <- rightOrThrow $ defaultConfig
  runGame @Yukon config
