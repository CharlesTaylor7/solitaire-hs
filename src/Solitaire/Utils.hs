module Solitaire.Utils where

import Solitaire.Imports
import Solitaire.Types
import qualified Data.Vector as V

cards :: Pile -> Vector Card
cards pile = up <> down
  where
    up = pile ^. faceUp
    down = pile ^. faceDown

emptyPile :: Pile
emptyPile = Pile V.empty V.empty
