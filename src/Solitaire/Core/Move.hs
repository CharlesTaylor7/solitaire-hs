module Solitaire.Core.Move where

import Solitaire.Prelude


class IsMove move game error | move -> error where
  apply :: MonadError error m => move -> game -> m game

