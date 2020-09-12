{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Solitaire.Yukon.Config where

import Solitaire.Prelude
import Solitaire.Core.Config
import Solitaire.Core.Types


defaultConfig :: MonadError InvalidConfig m => m Config
defaultConfig =
  mkConfig
    (NumSets 1)
    (Piles
      [ Pile 1 0
      , Pile 5 1
      , Pile 5 2
      , Pile 5 3
      , Pile 5 4
      , Pile 5 5
      , Pile 5 6
      ]
    )
