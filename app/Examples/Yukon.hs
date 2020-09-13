module Examples.Yukon where

import Solitaire.Yukon


main :: IO ()
main = do
  for_ (enumerate @[Card]) $ \card -> do
    prettyPrint card
    print card

  -- config <- rightOrThrow $ defaultConfig

  -- runGame @Yukon config
