module Solitaire.Core.Move where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  apply :: move -> game -> game
  moves :: game -> [move]


data SomeMoveType game =
  forall move. IsMove move game =>
    SomeMoveType (Proxy move)


data SomeMove game =
  forall move. IsMove move game =>
    SomeMove move


applySomeMove :: SomeMove game -> game -> game
applySomeMove (SomeMove move) = apply move


moveType :: forall move game. IsMove move game => SomeMoveType game
moveType = SomeMoveType (Proxy @move)
