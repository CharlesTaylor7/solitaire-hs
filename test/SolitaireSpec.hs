module SolitaireSpec where

import Solitaire
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Solitaire" $ do
    describe "deck" $ do
      it "should have the right number of cards" $ do
        config <- fromEither $ configWith (NumSets 3) (NumPiles 3) (NumFaceDown 2)

        let
          runConfig = flip runReader config
          numCards = enumSize @Card
          (deckSize, numCardCopies) = runConfig $ do
            d <- length <$> getDeck
            c <- view config_numSets
            pure (d, c)
          product = numCards * numCardCopies :: Int
        deckSize `shouldBe` product
