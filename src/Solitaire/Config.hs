{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Solitaire.Config where

import Solitaire.Imports
import Solitaire.Utils
import qualified Solitaire.Internal.Types as Internal (pattern Env)
import qualified Data.IntMap as M

newtype NumPiles = NumPiles Int
newtype NumFaceDown = NumFaceDown Int
newtype NumSets = NumSets Int
newtype Piles = Piles [PileCounts]

newtype InvalidConfig = InvalidConfig Text
  deriving (Eq, Show)

instance Exception InvalidConfig

envWith :: (MonadError InvalidConfig m) => NumSets -> NumPiles -> NumFaceDown -> m Env
envWith (NumSets s) (NumPiles p) (NumFaceDown f)
  | s < 0 = throwError $ InvalidConfig "Number of sets should be non negative."
  | p < 0 = throwError $ InvalidConfig "Number of piles should be non negative."
  | f * p > s * enumSize @Card = throwError $ InvalidConfig "Face down cards cannot outnumber deck size."
  | otherwise =
    let
      deckSize = s * enumSize @Card
      (pileSize, rem) = deckSize `divMod` p
      faceUpCount i = pileSize - f + if i < rem then 1 else 0
      pileCounts i = Pile { _faceUp = faceUpCount i, _faceDown = f }
    in pure Internal.Env
      { _env_numSets = s
      , _env_piles = M.fromAscList $
        map (id &&& pileCounts) [0..p-1]
      }

mkEnv :: (MonadError InvalidConfig m) => NumSets -> Piles -> m Env
mkEnv (NumSets s) (Piles piles)
  | s < 0 = throwError $ InvalidConfig "Number of sets should be non negative."
  | s * enumSize @Card /= (sum . map pileCountsSize) piles = throwError $ InvalidConfig "Pile layout doesn't match deck size."
  | otherwise = pure Internal.Env
    { _env_numSets = s
    , _env_piles = M.fromAscList $ zip [0..p-1] piles
    }
    where p = length piles
