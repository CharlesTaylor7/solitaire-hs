module Solitaire.Core.Move.Class where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  steps :: game -> [(move, game)]


type MoveConstraints move game = (IsMove move game, Show move)

data SomeMoveType game =
  forall move. MoveConstraints move game =>
    SomeMoveType (Proxy move)


data SomeMove game =
  forall move. MoveConstraints move game =>
    SomeMove move

deriving instance Show (SomeMove game)
deriving via WrappedShow (SomeMove game) instance Pretty (SomeMove game)


moveType :: forall move game. MoveConstraints move game => SomeMoveType game
moveType = SomeMoveType (Proxy @move)
