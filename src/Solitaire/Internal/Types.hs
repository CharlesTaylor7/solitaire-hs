{-# LANGUAGE TemplateHaskell #-}
module Solitaire.Internal.Types where

import RIO
import Control.Lens


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Config = Config
  { _config_numSets :: Int
  , _config_piles :: IntMap PileCounts
  }
  deriving (Eq, Show, Read)

data Pile a = Pile
  { _faceUp :: a
  , _faceDown :: a
  }
  deriving (Eq, Ord, Show, Read)

type PileCounts = Pile Int
type PileCards = Pile (Vector Card)

pileCounts :: Int -> Int -> PileCounts
pileCounts = Pile

pileCards :: Vector Card -> Vector Card -> PileCards
pileCards = Pile

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Ord, Show, Read)

data Foundation = Foundation
  { _numSets :: Int
  }
  deriving (Eq, Ord, Show, Read)

data Game = Game
  { _layout :: Layout
  , _foundation :: Foundation
  }
  deriving (Eq, Ord, Show, Read)

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
  | EmptyStackTarget Int
  | SourceIsTarget Int
  deriving (Eq, Show, Read)

instance Exception InvalidMove

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

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
