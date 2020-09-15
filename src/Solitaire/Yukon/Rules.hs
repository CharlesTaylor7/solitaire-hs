{-# language FlexibleInstances #-}
{-# options_ghc -Wwarn #-}
module Solitaire.Yukon.Rules
  ( Yukon
  ) where

import Solitaire.Prelude
import Solitaire.Core.Rules
import Solitaire.Core.Utils
  ( pileCountsSize
  , getDeck
  , totalCards
  , cards
  )

import Solitaire.Core.Card (splitAtFirstRun)
import Solitaire.Core.Utils (toPile)

import Solitaire.Yukon.PrettyInstances ()
import Solitaire.Yukon.Types hiding (Game, Config)
import qualified Solitaire.Yukon.Types as Yukon

import qualified Data.Vector as V
import qualified Data.IntMap as M


import Debug.Trace


data Yukon



instance Rules Yukon where

  type Game Yukon = Yukon.Game
  type Config Yukon = Yukon.Config
