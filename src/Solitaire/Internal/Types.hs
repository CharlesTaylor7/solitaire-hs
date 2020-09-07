{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Solitaire.Internal.Types where

import Prelude

import Control.Exception
import Control.Lens

import Data.Foldable
import Data.Hashable
import Data.Vector (Vector)
import Data.IntMap (IntMap)

import GHC.Generics (Generic)


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

data Config = Config
  { _config_numSets :: Int
  , _config_piles :: IntMap PileCounts
  }
  deriving (Eq, Show, Read)

data Pile a = Pile
  { _faceUp :: a
  , _faceDown :: a
  }
  deriving (Eq, Ord, Show, Read, Generic)

type PileCounts = Pile Int
type PileCards = Pile (Vector Card)

pileCounts :: Int -> Int -> PileCounts
pileCounts = Pile

pileCards :: Vector Card -> Vector Card -> PileCards
pileCards = Pile

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Foundation = Foundation
  { _numSets :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Game = Game
  { _layout :: Layout
  , _foundation :: Foundation
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Move
  = MoveStack MoveStack
  | FlipCard FlipCard
  | MoveToFoundation MoveToFoundation
  deriving (Eq, Show, Read)

data Step = Step
  { _step_move :: Move
  , _step_game :: Game
  }
  deriving (Eq, Read, Show)

data MoveStack = MS
  { _ms_fromIndex :: Int
  , _ms_toIndex :: Int
  }
  deriving (Eq, Show, Read)

newtype FlipCard = FC
  { _fc_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

newtype MoveToFoundation = MTF
  { _mtf_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

data InvalidMove
  = CardFlipOnUnexposedPile Int
  | CardFlipOnEmptyPile Int
  | IncompleteSet Int
  | MismatchingStacks Int Int
  | EmptyStackSource Int
  | MoveStackOntoFaceDownCards Int
  | SourceIsTarget Int
  deriving (Eq, Show, Read)

instance Exception InvalidMove

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

-- hashable instances
instance Hashable Game
instance Hashable Layout
instance Hashable Foundation
instance Hashable Card
instance Hashable a => Hashable (Pile a)

deriving via (SomeFoldable IntMap a) instance Hashable a => Hashable (IntMap a)
deriving via (SomeFoldable Vector a) instance Hashable a => Hashable (Vector a)

newtype SomeFoldable f a = SomeFoldable { getFoldable :: f a }

instance (Foldable f, Hashable a) => Hashable (SomeFoldable f a) where
  hashWithSalt salt = hashWithSalt salt . toList . getFoldable

flipCard :: Int -> Move
flipCard = FlipCard . FC

moveToFoundation :: Int -> Move
moveToFoundation = MoveToFoundation . MTF

moveStack :: Int -> Int -> Move
moveStack = (MoveStack .) . MS

makeLensesWith (lensRules & generateUpdateableOptics .~ False) ''Config
makePrisms ''Card
makePrisms ''Layout
makeLenses ''Foundation
makeLenses ''Game
makeLenses ''Pile
makePrisms ''Move
makeLenses ''Step
makeLenses ''MoveStack
makeLenses ''FlipCard
makeLenses ''MoveToFoundation
