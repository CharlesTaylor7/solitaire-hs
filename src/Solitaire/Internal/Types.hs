{-# LANGUAGE TemplateHaskell #-}
module Solitaire.Internal.Types where

import RIO
import Control.Exception
import Control.Lens
import Control.Monad.Random
import Control.Monad.State.Strict

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
  deriving (Eq, Show, Read)

type PileCounts = Pile Int
type PileCards = Pile (Vector Card)

pileCounts :: Int -> Int -> PileCounts
pileCounts = Pile

pileCards :: Vector Card -> Vector Card -> PileCards
pileCards = Pile

newtype Layout = Layout
  { unLayout :: IntMap PileCards
  }
  deriving (Eq, Show, Read)

data Foundation = Foundation
  { _numSets :: Int
  }
  deriving (Eq, Show, Read)

data Game = Game
  { _layout :: Layout
  , _foundation :: Foundation
  }
  deriving (Eq, Show, Read)

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

flipCard = FlipCard . FC
moveToFoundation = MoveToFoundation . MTF
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

-- random instances
randomS :: (RandomGen g, Random a) => State g a
randomS = state random

instance Random Move where
  random = runState $ do
    let numMoveTypes = 3 :: Int
    choice <- (`mod` numMoveTypes) <$> randomS
    case choice of
      0 -> MoveStack <$> randomS
      1 -> FlipCard <$> randomS
      2 -> MoveToFoundation <$> randomS
  randomR _ = random

instance Random MoveStack where
  random = runState $ MS <$> randomS <*> randomS
  randomR _ = random

instance Random FlipCard where
  random = runState $ FC <$> randomS
  randomR _ = random

instance Random MoveToFoundation where
  random = runState $ MTF <$> randomS
  randomR _ = random
