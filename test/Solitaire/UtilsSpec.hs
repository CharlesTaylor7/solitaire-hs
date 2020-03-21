{-# LANGUAGE NoOverloadedLists #-}
module Solitaire.UtilsSpec where

import Data.Monoid

import Control.Monad.Identity
import Control.Monad.State
import Test.QuickCheck
import Test.Hspec

import Solitaire

data DummyEnum = A | B | C
  deriving (Enum, Bounded)

newtype Line = Line String

instance Semigroup Line where
  Line "" <> x = x
  x <> Line "" = x
  Line x <> Line y = Line $ x <> y

instance Monoid Line where
  mempty = Line ""

take :: Int -> ListT m a -> m [a]
take = undefined

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

    describe "loopM'" $ do
      it "enables backtracking with a list monad transformer" $ do
        let
          writeLine :: MonadWriter Line m => String -> m ()
          writeLine = tell . Line

          sequenceL :: Monad m => [ListT m a] -> ListT m a
          sequenceL = join . Select . each

          act :: (MonadError Int m, MonadWriter Line m)
              => Int
              -> ListT m Int
          act x =
            let
              case1 = sequenceL
                [
                  writeLine "Try x-1" >> pure (x-1),
                  writeLine "Try 4*x" >> pure (4*x)
                ]
              case2 = sequenceL
                [
                  writeLine "Try x + 10" >> pure (x + 10),
                  writeLine "Try x / 30" >> pure (x `div` 30)
                ]
              case3 = sequenceL
                [
                  writeLine "Try x*1337 mod 779" >> pure ( x *1337 `mod` 779)
                ]
            in
              sequenceL [case1, case2, case3]

          result = loopM' act 0
          r' = runErrorT result
        2 `shouldBe` 2
