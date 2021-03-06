module Solitaire.Core.Config where

import Solitaire.Prelude
import Solitaire.Core.Types
import Solitaire.Core.Utils (pileCountsSize)

import qualified Data.IntMap as M

import Debug.Trace


newtype NumPiles = NumPiles Int
newtype NumFaceDown = NumFaceDown Int
newtype NumSets = NumSets Int
newtype Piles = Piles [Pile Int]

newtype InvalidConfig = InvalidConfig Text
  deriving (Eq, Show)

instance Exception InvalidConfig

type BoundedEnum a = (Bounded a, Enum a)

mkConfig :: forall card m. (MonadError InvalidConfig m, BoundedEnum card) => NumSets -> Piles -> m GameConfig
mkConfig (NumSets s) (Piles piles)
  | s < 0 = throwError $ InvalidConfig "Number of sets should be non negative."
  | otherwise = do
      let
        requestedCardCount = s * enumSize @card
        totalPileAmount = piles
          & sumOf (folded . to pileCountsSize)

        p = length piles

      when (requestedCardCount /= totalPileAmount) $

        throwError $ InvalidConfig "Pile layout doesn't match deck size."
          & traceShow (totalPileAmount, requestedCardCount)

      pure GameConfig
        { numSets = s
        , piles = M.fromAscList $ zip [0..p-1] piles
        }


configWith :: (MonadError InvalidConfig m) => NumSets -> NumPiles -> NumFaceDown -> m GameConfig
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
    in pure GameConfig
      { numSets = s
      , piles = M.fromAscList $
        map (id &&& pileCounts) [0..p-1]
      }

