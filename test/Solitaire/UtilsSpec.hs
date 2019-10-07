{-# LANGUAGE TypeApplications #-}
module Solitaire.UtilsSpec where

import Solitaire
import Test.QuickCheck
import Test.Hspec

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
