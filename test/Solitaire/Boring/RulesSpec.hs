module Solitaire.Boring.RulesSpec where

import Test.Hspec

import Solitaire.Boring

import qualified Data.IntMap as M

spec :: Spec
spec = do
  describe "Actions" $ do
    describe "scorePile" $ do
      it "treats single card as a run of score 0" $
        let pile = Pile [Four] []
        in scorePile pile `shouldBe` 0
      it "handles multiple runs" $
        let pile = Pile [One, Two, Four, Five, Three] []
        in scorePile pile `shouldBe` 2
      it "ignores face down cards" $
        let pile = Pile [] [One, Two, Three, Two]
        in scorePile pile `shouldBe` 0

    describe "getDeck" $ do
      it "should have the right number of cards" $ do
        config <- rightOrThrow $ configWith (NumSets 3) (NumPiles 3) (NumFaceDown 2)

        let
          runConfig = flip runReader config
          numCards = enumSize @Card
          (deckSize, numCardCopies) = runConfig $ do
            d <- length <$> getDeck
            c <- view #numSets
            pure (d, c)
          product = numCards * numCardCopies :: Int
        deckSize `shouldBe` product

    describe "moveReducer" $ do
      describe "FlipCard" $ do
        it "flips a card from face down to face up in a pile" $ do
          let move = flipCard 1
          let game = Game
                { layout = Layout
                  (M.empty & at 1 .~
                    (Just (Pile
                      { faceUp = []
                      , faceDown = [One]
                      })
                    )
                  )
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Game {layout = Layout {unLayout = [(1,Pile {faceUp = [One], faceDown = []})]}, foundation = Foundation {numSets = 0}}
          let result = moveReducer move game
          result `shouldBe` Right expected
        it "marks flipping a face down card on an unexposed pile as invalid" $ do
          let move = flipCard 1
          let game = Game
                { layout = Layout
                  (M.empty & at 1 .~
                    (Just (Pile
                      { faceUp = [Two]
                      , faceDown = [One]
                      })))
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left (CardFlipOnUnexposedPile 1)
          let result = moveReducer move game
          result `shouldBe` expected
        it "marks flipping a face down card on an empty pile as invalid" $ do
          let move = flipCard 1
          let game = Game
                { layout = Layout
                  (M.empty & at 1 .~ (Just
                    (Pile
                      { faceUp = []
                      , faceDown = []
                      })
                  ))
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left (CardFlipOnEmptyPile 1)
          let result = moveReducer move game
          result `shouldBe` expected
      describe "MoveToFoundation" $ do
        it "moves a complete set from the layout to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { faceUp = enumerate @Card
                      , faceDown = []
                      })
                  ))
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Right $ Game
                { layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { faceUp = []
                      , faceDown = []
                      })
                  ))
                , foundation = Foundation { numSets = 1 }
                }
          let result = moveReducer move game
          result `shouldBe` expected
        it "moves a complete set from the layout to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { faceUp = enumerate @Card
                      , faceDown = []
                      })
                  ))
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Right $ Game
                { layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { faceUp = []
                      , faceDown = []
                      })
                  ))
                , foundation = Foundation { numSets = 1 }
                }
          let result = moveReducer move game
          result `shouldBe` expected
        it "cannot move an incomplete set to the foundation" $ do
          let move = moveToFoundation 2
          let game = Game
                { layout = Layout
                  (M.empty & at 2 .~ (Just
                    (Pile
                      { faceUp = []
                      , faceDown = [One]
                      })
                  ))
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left $ IncompleteSet 2
          let result = moveReducer move game
          result `shouldBe` expected
      describe "MoveStack" $ do
        it "moves stacks of cards" $ do
          let move = moveStack 0 2
          let game = Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [One, Two, Five]
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = [Three, Two, Five]
                      , faceDown = [Two, One]
                      })
                   ]
                , foundation = Foundation { numSets = 0 }
                }
          let
            expected :: Either InvalidMove Game
            expected = Right $ Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [Five]
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = [One, Two, Three, Two, Five]
                      , faceDown = [Two, One]
                      })
                  ]
                , foundation = Foundation { numSets = 0 }
                }
          let result = moveReducer move game
          (result `shouldBe` expected)
            & tracePretty result
            & tracePretty expected
        it "cannot move an empty stack" $ do
          let move = moveStack 0 2
          let game = Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = []
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = [Three, Two, Five]
                      , faceDown = [Two, One]
                      })
                    ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left $ EmptyStackSource 0
          let result = moveReducer move game
          result `shouldBe` expected
        it "can move onto an empty pile" $ do
          let move = moveStack 0 2
          let game = Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [Five]
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = []
                      , faceDown = []
                      })
                  ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Right $  Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = []
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = [Five]
                      , faceDown = []
                      })
                  ]
                , foundation = Foundation { numSets = 0 }
                }
          let result = moveReducer move game
          result `shouldBe` expected
        it "cannot move onto face down cards" $ do
          let move = moveStack 0 2
          let game = Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [Five]
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = []
                      , faceDown = [Two, One]
                      })
                    ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left $ MoveStackOntoFaceDownCards 2
          let result = moveReducer move game
          result `shouldBe` expected

        it "cannot move stack onto a mismatching card" $ do
          let move = moveStack 0 2
          let game = Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [Five]
                      , faceDown = [Three]
                      })
                  , (2, Pile
                      { faceUp = [One]
                      , faceDown = [Two, One]
                      })
                    ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left $ MismatchingStacks 0 2
          let result = moveReducer move game
          result `shouldBe` expected
      it "cannot move stack onto itself" $ do
          let move = moveStack 1 1
          let game = Game
                { layout = Layout
                  [ (1, Pile
                      { faceUp = [Five]
                      , faceDown = [Three]
                      })
                  ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Left $ SourceIsTarget 1
          let result = moveReducer move game
          result `shouldBe` expected
      it "can move stack which splits a stack" $ do
          let move = moveStack 1 2
          let game = Game
                { layout = Layout
                  [ (1, Pile
                      { faceUp = [Four, Five, One, Two]
                      , faceDown = []
                      })
                  , (2, Pile
                      { faceUp = [Five]
                      , faceDown = [Two, One]
                      })
                    ]
                , foundation = Foundation { numSets = 0 }
                }
          let expected = Right $  Game
                { layout = Layout
                  [ (0, Pile
                      { faceUp = [Five]
                      , faceDown = []
                      })
                  , (1, Pile
                      { faceUp = [Four, Five, One, Two]
                      , faceDown = [Two, One]
                      })
                  ]
                , foundation = Foundation { numSets = 0 }
                }
          let result = moveReducer move game
          result `shouldBe` expected
