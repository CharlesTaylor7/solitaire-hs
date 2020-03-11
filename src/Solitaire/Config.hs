{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Config where

import Solitaire.Imports
import qualified Solitaire.Internal.Types as Internal (pattern Env)

newtype NumPiles = NumPiles Int
newtype NumFaceDown = NumFaceDown Int
newtype NumSets = NumSets Int
newtype Piles = Piles [PileCounts]

envWith :: NumSets -> NumPiles -> NumFaceDown -> Env
envWith (NumSets s) (NumPiles p) (NumFaceDown f) = Internal.Env {}

mkEnv :: NumSets -> Piles -> Maybe Env
mkEnv = undefined
