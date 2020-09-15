module Solitaire.Core.Move where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  apply :: move -> game -> Either (InvalidMove move) game
  moves :: NumPiles -> [move]


type family InvalidMove (move :: *) = (invalid :: *) | invalid -> move


type IsMoveConstraints move game =
  ( IsMove move game
  , Exception (InvalidMove move)
  )


