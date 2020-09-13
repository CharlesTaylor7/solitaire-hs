module Solitaire.Core.Utils where

import Solitaire.Prelude
import Solitaire.Core.Types


color :: Suit -> Color
color Hearts = Red
color Diamonds = Red
color _ = Black


isSuccessorOf :: Card -> Card -> Bool
a `isSuccessorOf` b =
  -- a has a rank 1 higher than b
  a ^. #rank . from enum - b ^. #rank . from enum == 1 &&
  -- a is the opposite color of b
  a ^. #suit . to color /= b ^. #suit . to color


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
