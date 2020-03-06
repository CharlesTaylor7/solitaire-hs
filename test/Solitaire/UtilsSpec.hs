module Solitaire.UtilsSpec where

import Data.Monoid

import Control.Monad.Identity
import Control.Monad.State
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
        enumSize @DummyEnum `shouldBe` 3
    describe "loopM" $ do
      it "enables while loops in the identity monad" $ do
        let factorial (product, n) =
             if n <= 0
             then pure $ Left product
             else pure $ Right (n * product, n - 1)
        loopM factorial (1, 3) `shouldBe` Identity 6
      it "enables imperative style accumulation while loops in the writer monad" $ do
        let factorial x =
              if x <= 0
              then pure $ Left ()
              else (Product x, Right (x - 1))
        loopM factorial 4 `shouldBe` (Product 24, ())
      it "enables branching in the list monad" $ do
        let
          act :: Int -> [Either Int Int]
          act x =
            if x <= 0
            then [Left (x + 1)]
            else
              [ Right (x - 1)
              , Right (x - 2)
              , Left (x + 1)
              ]
        let expected = [1, 0, 2, 1, 3, 1, 0, 2, 4] :: [Int]
        loopM act 3 `shouldBe` expected
