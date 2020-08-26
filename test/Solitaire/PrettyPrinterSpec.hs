module Solitaire.PrettyPrinterSpec where

import Solitaire

import Test.Hspec


spec :: Spec
spec = do
  describe "Pretty typeclass; it prints ascii art of the game state" $ do
    it "prints the game" $ do
      let game = Game {_layout = Layout {unLayout = [(0,Pile
      {_faceUp = [One,Five], _faceDown = [Four,One,Five]}),(1,Pile {_faceUp = [Four,Two], _faceDown = [Five,Two,One]}),(2,Pile {_faceUp = [Four,Three], _faceDown = [Two,Three,Three]})]}, _foundation = Foundation {_numSets = 0}}
      pretty game `shouldBe` "[0]\n-|-|-\n-|-|-\n-|-|-\n5|2|3\n1|4|4"
    it "prints uneven piles" $ do
      let game = Game {_layout = Layout {unLayout = [(0,Pile
      {_faceUp = [Five], _faceDown = [Four,One,Five]}),(1,Pile {_faceUp = [Four,Two], _faceDown = [Five,Two,One, Five]}),(2,Pile {_faceUp = [Four,Three], _faceDown = [Two,Three]})]}, _foundation = Foundation {_numSets = 1}}
      pretty game `shouldBe`  "[1]\n-|-|-\n-|-|-\n-|-|3\n5|-|4\n |2| \n |4| "
