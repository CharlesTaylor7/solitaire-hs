{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Solitaire.Boring.Config where

import Solitaire.Prelude
import Solitaire.Boring.Utils
import Solitaire.Boring.Types

import qualified Solitaire.Boring.Types as Internal (pattern Config)
import qualified Data.IntMap as M

newtype NumPiles = NumPiles Int
newtype NumFaceDown = NumFaceDown Int
newtype NumSets = NumSets Int
newtype Piles = Piles [PileCounts]

newtype InvalidConfig = InvalidConfig Text
  deriving (Eq, Show)

instance Exception InvalidConfig

boringSolitaireConfig :: Config
boringSolitaireConfig =
  let
    Right config = configWith (NumSets 2) (NumPiles 5) (NumFaceDown 1)
  in config

configWith :: (MonadError InvalidConfig m) => NumSets -> NumPiles -> NumFaceDown -> m Config
configWith (NumSets s) (NumPiles p) (NumFaceDown f)
  | s < 0 = throwError $ InvalidConfig "Number of sets should be non negative."
  | p < 0 = throwError $ InvalidConfig "Number of piles should be non negative."
  | f * p > s * enumSize @Card = throwError $ InvalidConfig "Face down cards cannot outnumber deck size."
  | otherwise =
    let
      deckSize = s * enumSize @Card
      (pileSize, rem) = deckSize `divMod` p
      faceUpCount i = pileSize - f + if i < rem then 1 else 0
      pileCounts i = Pile { faceUp = faceUpCount i, faceDown = f }
    in pure Internal.Config
      { numSets = s
      , piles = M.fromAscList $
        map (id &&& pileCounts) [0..p-1]
      }

mkConfig :: (MonadError InvalidConfig m) => NumSets -> Piles -> m Config
mkConfig (NumSets s) (Piles piles)
  | s < 0 = throwError $ InvalidConfig "Number of sets should be non negative."
  | s * enumSize @Card /= (sum . map pileCountsSize) piles = throwError $ InvalidConfig "Pile layout doesn't match deck size."
  | otherwise = pure Internal.Config
    { numSets = s
    , piles = M.fromAscList $ zip [0..p-1] piles
    }
    where p = length piles
