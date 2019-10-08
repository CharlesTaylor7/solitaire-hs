module Solitaire.PrettyPrinterSpec where

import Solitaire
import Solitaire.PrettyPrinter

import Data.IntMap (fromList)
import qualified Data.Vector as V

import Test.Hspec

spec = do
  describe "Pretty typeclass; it prints ascii art of the game state" $ do
    it "prints the game" $ do
      let game = Game {_layout = Layout {unLayout = fromList [(0,Pile
      {_faceUp = [One,Five], _faceDown = [Four,One,Five]}),(1,Pile {_faceUp = [Four,Two], _faceDown = [Five,Two,One]}),(2,Pile {_faceUp = [Four,Three], _faceDown = [Two,Three,Three]})]}, _foundation = Foundation {_numSets = 0}}
      pretty game `shouldBe` "[0]\n-|-|-\n-|-|-\n-|-|-\n1|4|4\n5|2|3"
    it "prints uneven piles" $ do
      let game = Game {_layout = Layout {unLayout = fromList [(0,Pile
      {_faceUp = [Five], _faceDown = [Four,One,Five]}),(1,Pile {_faceUp = [Four,Two], _faceDown = [Five,Two,One, Five]}),(2,Pile {_faceUp = [Four,Three], _faceDown = [Two,Three]})]}, _foundation = Foundation {_numSets = 1}}
      pretty game `shouldBe` "[1]\n-|-|-\n-|-|-\n-|-|4\n5|-|3\n |4| \n |2| "
