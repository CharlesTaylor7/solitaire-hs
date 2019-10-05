module SolitaireSpec where

import Solitaire
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Solitaire" $
    describe "deck" $
      it "has more than zero cards" 
