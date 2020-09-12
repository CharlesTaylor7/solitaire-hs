module Solitaire.Boring.Config
  ( module CoreConfig
  , boringSolitaireConfig
  , staggeredConfig
  ) where

import Solitaire.Prelude
import Solitaire.Core.Config as CoreConfig
import Solitaire.Boring.Types

import System.IO.Unsafe (unsafePerformIO)


boringSolitaireConfig :: GameConfig
boringSolitaireConfig =
  let
    Right config = configWith (NumSets 2) (NumPiles 5) (NumFaceDown 1)
  in config

staggeredConfig :: GameConfig
staggeredConfig = unsafePerformIO $ do
  config <- rightOrThrow $ mkConfig @Card
    (NumSets 2)
    (Piles
      [ Pile 1 0
      , Pile 1 1
      , Pile 1 2
      , Pile 1 3
      ]
    )
  pure config

