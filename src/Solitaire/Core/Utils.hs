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
