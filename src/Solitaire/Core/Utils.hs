module Solitaire.Core.Utils where

import Solitaire.Prelude
import Solitaire.Core.Types
import Solitaire.Core.Card


color :: Suit -> Color
color Hearts = Red
color Diamonds = Red
color _ = Black


pileCountsSize :: Pile Int -> Int
pileCountsSize counts = counts ^. #faceUp + counts ^. #faceDown


cards :: Semigroup a => Pile a -> a
cards pile = pile ^. #faceUp <> pile ^. #faceDown


emptyPile :: Monoid a => Pile a
emptyPile = Pile mempty mempty


getDeck :: (Enum card, Bounded card) => MonadReader Config m => m [card]
getDeck = do
  numSets <- view #numSets
  pure $ enumerate >>= replicate numSets


toPile :: (IsList l, Item l ~ card) => [card] -> Pile Int -> Pile l
toPile cards counts =
  let
    n = counts ^. #faceUp
    (up, down) = splitAt n cards
  in
    Pile (fromList up) (fromList down)


pileSize :: Foldable f => Pile (f card) -> Int
pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)


totalCards :: Tableau card -> Int
totalCards = sumOf (#_Tableau . folded . to pileSize)


cardsRemaining :: Game c f s -> MoveCount
cardsRemaining = view
  $ #tableau
  . #_Tableau
  . folded -- piles
  . folded -- vectors
  . to (MoveCount . length)


scoreByRuns :: IsCard card => Tableau card -> Score
scoreByRuns game =
  game & sumOf (#_Tableau . folded . to scorePile)

scorePile :: (IsCard card, Foldable f) => Pile (f card) -> Score
scorePile pile =
  pile & sumOf (#faceUp . to toList . to splitIntoRuns . traverse . to scoreRun)

scoreRun :: Run card -> Score
scoreRun (Run cards) = Score $ length cards - 1
