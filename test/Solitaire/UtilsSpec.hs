{-# LANGUAGE TypeApplications #-}
module Solitaire.UtilsSpec where

import Data.Monoid

import Control.Monad.Trans.State.Strict
import Test.QuickCheck
import Test.Hspec

import Solitaire

data DummyEnum = A | B | C
  deriving (Enum, Bounded)

spec = do
  describe "Utils" $ do
    describe "chunksOf" $ do
      it "handles empty lists" $ do
        let emptyList = [] :: [Int]
        chunksOf 3 emptyList `shouldBe` []
      it "splits lists into even chunks" $ do
        chunksOf 2 [1..10] `shouldBe` [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
      it "assigns the remainder to the final chunk" $ do
        chunksOf 3 [1..5] `shouldBe` [[1, 2, 3], [4, 5]]
    describe "enumSize" $ do
      it "gets the size of a bounded enum" $ do
        enumSize @DummyEnum A `shouldBe` 3
      it "does not evaluate its argument" $ do
        enumSize @DummyEnum undefined `shouldBe` 3
    describe "loopM" $ do
      it "enables branching in the list monad" $ do
        let act x =
              if x <= 0
              then [Right (x + 1)]
              else
                [ Left (x - 1)
                , Left (x - 2)
                , Right (x + 1)
                ]

        loopM act 3 `shouldBe` [1, 0, 2, 1, 3, 1, 0, 2, 4]
      it "enables imperative style while loops in the writer monad" $ do
        let factorial x =
              if x <= 0
              then pure $ Right ()
              else (Product x, Left (x - 1))
        loopM factorial 4 `shouldBe` (Product 24, ())
      it "what does it do in the reader monad?" $ do
        let try x = \n ->
              if x > 10
              then Right x
              else Left (n+x)
        (loopM try 2) 4 `shouldBe` 14
