module Solitaire.Core.Move where

import Solitaire.Prelude
import Solitaire.Core.Config


class IsMove move game where
  type InvalidMove move :: *
  apply :: MonadError (InvalidMove move) m => move -> game -> m game
  moves :: NumPiles -> [move]
