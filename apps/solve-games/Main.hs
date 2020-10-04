{-# options_ghc -Wwarn #-}
module Main where

import Solitaire.Yukon

import System.Directory
import qualified Data.UUID as UUID


main :: IO ()
main = do
  gameConfig <- rightOrThrow $ defaultConfig

  let
    statsConfig = StatsConfig
      { numTrials = 50
      , microSecondsTimeout = 10 * (10 ^ 6)
      , heuristicWeights = mempty
        & at "numFaceUp" ?~ 2
        & at "numFaceDown" ?~ 5
        & at "totalRunScore" ?~ (-0.5)
      }
    appConfig = AppConfig statsConfig gameConfig

  games <- runTrials @Yukon appConfig
  prettyPrint games

  writeToFiles games


writeToFiles :: (Show k, Show a, Read a) => Map k [a] -> IO ()
writeToFiles map = do
  ifor_ map $ \k as -> do
    let dirName = "games/" <> show k <> "/"

    createDirectoryIfMissing True dirName
    for_ as $ \a -> do
      uuid <- getRandom

      let filePath = dirName <> UUID.toString uuid

      appendFile filePath (show a)
