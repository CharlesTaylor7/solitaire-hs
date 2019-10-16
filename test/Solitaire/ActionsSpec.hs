module Solitaire.ActionsSpec where

import Test.QuickCheck
import Test.Hspec

import Solitaire
import qualified Data.IntMap as M
import qualified Data.Vector as V

import Control.Lens

spec = do
  describe "Actions" $ do
    describe "moveReducer" $ do
      it "can flip a face down card in a pile" $ do
        let move = flipCard 1
        let game = Game
              { _layout = Layout
                (M.empty & at 1 .~
                  (Just (Pile
                    { _faceUp = V.empty
                    , _faceDown = [One]
                    })
                  )
                )
              , _foundation = Foundation { _numSets = 0 }
              }
        let expected = Right (Game {_layout = Layout {unLayout = [(1,Pile {_faceUp = [One], _faceDown = []})]}, _foundation = Foundation {_numSets = 0}})
        moveReducer move game `shouldBe` expected
      it "marks flipping a face down card on an unexposed pile as invalid" $ do
        let move = flipCard 1
        let game = Game
              { _layout = Layout
                (M.empty & at 1 .~
                  (Just (Pile
                    { _faceUp = [Two]
                    , _faceDown = [One]
                    })))
              , _foundation = Foundation { _numSets = 0 }
              }
        let expected = Left (CardFlipOnUnexposedPileError 1)
        moveReducer move game `shouldBe` expected
      it "marks flipping a face down card on an empty pile as invalid" $ do
        let move = flipCard 1
        let game = Game
              { _layout = Layout
                (M.empty & at 1 .~ (Just
                  (Pile
                    { _faceUp = []
                    , _faceDown = []
                    })
                ))
              , _foundation = Foundation { _numSets = 0 }
              }
        let expected = Left (CardFlipOnEmptyPileError 1)
        moveReducer move game `shouldBe` expected
