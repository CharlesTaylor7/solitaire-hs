module SolitaireSpec where

import Solitaire
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Solitaire" $ do
    describe "deck" $ do
      it "should have the right number of cards" $ do
        let env = envWith (NumSets 3) (NumPiles 3) (NumFaceDown 2)
        let runEnv = flip runReader env
        let numCards = enumSize @Card
        let
          (deckSize, numCardCopies) = runEnv $ do
            d <- length <$> getDeck
            c <- view env_numSets
            pure (d, c)
        let product = numCards * numCardCopies :: Int
        deckSize `shouldBe` product
