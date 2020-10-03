{-# options_ghc -Wwarn #-}
module Main where

import Solitaire.Yukon
import System.Directory
import System.Microtimer


main :: IO ()
main = do
  files <- listDirectory "games/Solvable"
  for_ files $ \filePath -> do
    file <- readFile $ "games/Solvable/" <> filePath
    let
      game :: Game
      game = read file
      weights = mempty
        & at "numFaceUp" ?~ 2
        & at "numFaceDown" ?~ 5
        & at "totalRunScore" ?~ (-0.5)

    time <- runGame @Yukon game
      & (runReaderT ?? weights)
      & time_

    putStrLn filePath
    putStrLn $ formatSeconds time

    prettyPrint game


updateWeightsAtRandom
  :: forall k m. (Ord k, MonadRandom m)
  => Map k Float
  -> m (Map k Float)
updateWeightsAtRandom map = do
  let
    n = length map
    keys = (map ^.. ifolded . asIndex)

  delta <- randomDelta n

  pure $ flip execState map $
    ifor_ keys $ \i key -> do
      ix key += (delta ^?! ix i)
