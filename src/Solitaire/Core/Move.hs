module Solitaire.Core.Move where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  data InvalidMove move :: *

  apply :: move -> game -> Either (InvalidMove move) game
  moves :: NumPiles -> [move]


type IsMoveConstraints move game =
  ( IsMove move game
  , Exception (InvalidMove move)
  )


data SomeMoveType game =
  forall move. IsMoveConstraints move game =>
    SomeMoveType (Proxy move)


data SomeMove game =
  forall move. IsMoveConstraints move game =>
    SomeMove move


applySomeMove :: (MonadError SomeException m) => SomeMove game -> game -> m game
applySomeMove (SomeMove move) =
  liftEither .
  over _Left SomeException .
  apply move


moveType :: forall move game. IsMoveConstraints move game => SomeMoveType game
moveType = SomeMoveType (Proxy @move)
