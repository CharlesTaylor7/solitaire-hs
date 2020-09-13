module Solitaire.Boring.Config
  ( module CoreConfig
  , boringSolitaireConfig
  ) where

import Solitaire.Prelude
import Solitaire.Core.Config as CoreConfig
import Solitaire.Core.Types


boringSolitaireConfig :: Config
boringSolitaireConfig =
  let
    Right config = configWith (NumSets 2) (NumPiles 5) (NumFaceDown 1)
  in config
