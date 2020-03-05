module Solitaire.ActionsSpec where

import Test.QuickCheck
import Test.Hspec

import Solitaire
import qualified Data.IntMap as M
import qualified Data.Vector as V

spec = do
  describe "Actions" $ do
    describe "moveReducer" $ do
      describe "FlipCard" $ do
        it "flips a card from face down to face up in a pile" $ do
          let move = flipCard 1
          let game = Game
                { _layout = Layout
                  (M.empty & at 1 .~
                    (Just (Pile
                      { _faceUp = []
                      , _faceDown = [One]
                      })
                    )
                  )
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Game {_layout = Layout {unLayout = [(1,Pile {_faceUp = [One], _faceDown = []})]}, _foundation = Foundation {_numSets = 0}}
          let result = moveReducer move game
          result `shouldBe` Right expected

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
          let expected = Left (CardFlipOnUnexposedPile 1)
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
          let expected = Left (CardFlipOnEmptyPile 1)
          moveReducer move game `shouldBe` expected
      describe "MoveToFoundation" $ do
        it "moves a complete set from the layout to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { _layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { _faceUp = enumerate @Card
                      , _faceDown = []
                      })
                  ))
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Right $ Game
                { _layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { _faceUp = []
                      , _faceDown = []
                      })
                  ))
                , _foundation = Foundation { _numSets = 1 }
                }
          moveReducer move game `shouldBe` expected
        it "moves a complete set from the layout to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { _layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { _faceUp = enumerate @Card
                      , _faceDown = []
                      })
                  ))
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Right $ Game
                { _layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { _faceUp = []
                      , _faceDown = []
                      })
                  ))
                , _foundation = Foundation { _numSets = 1 }
                }
          moveReducer move game `shouldBe` expected
        it "cannot move an incomplete set to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { _layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { _faceUp = []
                      , _faceDown = [One]
                      })
                  ))
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Left $ IncompleteSet 2
          moveReducer move game `shouldBe` expected
      describe "MoveStack" $ do
        it "moves stacks of cards" $ do
          let move = moveStack 0 2
          let game = Game
                { _layout = Layout
                  [ (0, Pile
                      { _faceUp = [One, Two, Five]
                      , _faceDown = [Three]
                      })
                  , (2, Pile
                      { _faceUp = [Three, Two, Five]
                      , _faceDown = [Two, One]
                      })
                   ]
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Right $ Game
                { _layout = Layout
                  [ (0, Pile
                      { _faceUp = [Five]
                      , _faceDown = [Three]
                      })
                  , (2, Pile
                      { _faceUp = [One, Two, Three, Two, Five]
                      , _faceDown = [Two, One]
                      })
                  ]
                , _foundation = Foundation { _numSets = 0 }
                }
          moveReducer move game `shouldBe` expected
        it "cannot move an empty stack" $ do
          let move = moveStack 0 2
          let game = Game
                { _layout = Layout
                  [ (0, Pile
                      { _faceUp = []
                      , _faceDown = [Three]
                      })
                  , (2, Pile
                      { _faceUp = [Three, Two, Five]
                      , _faceDown = [Two, One]
                      })
                    ]
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Left $ EmptyStackSource 0
          moveReducer move game `shouldBe` expected
        it "cannot move onto an empty stack" $ do
          let move = moveStack 0 2
          let game = Game
                { _layout = Layout
                  [ (0, Pile
                      { _faceUp = [Five]
                      , _faceDown = [Three]
                      })
                  , (2, Pile
                      { _faceUp = []
                      , _faceDown = [Two, One]
                      })
                    ]
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Left $ EmptyStackTarget 2
          moveReducer move game `shouldBe` expected
        it "cannot move stack onto a mismatching card" $ do
          let move = moveStack 0 2
          let game = Game
                { _layout = Layout
                  [ (0, Pile
                      { _faceUp = [Five]
                      , _faceDown = [Three]
                      })
                  , (2, Pile
                      { _faceUp = [One]
                      , _faceDown = [Two, One]
                      })
                    ]
                , _foundation = Foundation { _numSets = 0 }
                }
          let expected = Left $ MismatchingStacks 0 2
          let result = moveReducer move game
          result `shouldBe` expected
