module SolitaireSpec where

import Solitaire
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Solitaire" $ do
    describe "deck" $ do
      it "should have the right number of cards" $ do
        let deckSize = length deck
        deckSize `shouldBe` (numCards * numCardCopies)
