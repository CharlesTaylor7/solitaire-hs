{-# LANGUAGE PatternSynonyms #-}
module Solitaire.Config where

import Solitaire.Imports
import Solitaire.Utils
import qualified Solitaire.Internal.Types as Internal (pattern Env)
import qualified Data.IntMap as M


newtype NumPiles = NumPiles Int
newtype NumFaceDown = NumFaceDown Int
newtype NumSets = NumSets Int
newtype Piles = Piles [PileCounts]

envWith :: NumSets -> NumPiles -> NumFaceDown -> Maybe Env
envWith (NumSets s) (NumPiles p) (NumFaceDown f)
  | s < 0 = Nothing
  | p < 0 = Nothing
  | f * p > s * enumSize @Card = Nothing
  | otherwise =
    let
      deckSize = s * enumSize @Card
      (pileSize, rem) = deckSize `divMod` p
      faceUpCount i = pileSize - f + if i < rem then 1 else 0
      pileCounts i = Pile { _faceUp = faceUpCount i, _faceDown = f }
    in Just Internal.Env
      { _env_numSets = s
      , _env_piles = M.fromAscList $
        map (id &&& pileCounts) [0..p-1]
      }

mkEnv :: NumSets -> Piles -> Maybe Env
mkEnv (NumSets s) (Piles piles)
  | s < 0 = Nothing
  | s * enumSize @Card /= (sum . map pileCountsSize) piles = Nothing
  | otherwise = Just Internal.Env
    { _env_numSets = s
    , _env_piles = M.fromAscList $ zip [0..p-1] piles
    }
    where p = length piles
