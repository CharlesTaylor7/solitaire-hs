{-# options_ghc -Wwarn #-}
module Main where

import Solitaire.Prelude

import qualified Examples.Boring as Boring
import qualified Examples.Yukon as Yukon


main :: IO ()
main = do
--  Boring.main
  Yukon.main
