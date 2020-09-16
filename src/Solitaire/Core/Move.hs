module Solitaire.Core.Move where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  steps :: game -> [(move, game)]


data SomeMoveType game =
  forall move. IsMove move game =>
    SomeMoveType (Proxy move)


data SomeMove game =
  forall move. IsMove move game =>
    SomeMove move


moveType :: forall move game. IsMove move game => SomeMoveType game
moveType = SomeMoveType (Proxy @move)
