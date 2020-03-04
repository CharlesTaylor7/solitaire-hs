{-# LANGUAGE TemplateHaskell #-}
module Solitaire.Types where

import Solitaire.Imports

data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Pile = Pile
  { _faceUp :: Vector Card
  , _faceDown :: Vector Card
  }
  deriving (Eq, Show, Read)

newtype Layout = Layout
  { unLayout :: IntMap Pile
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

data MoveStack = MS
  { _ms_fromIndex :: Int
  , _ms_toIndex :: Int
  }
  deriving (Eq, Show, Read)

data FlipCard = FC
  { _fc_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

data MoveToFoundation = MTF
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
  deriving (Read, Show, Eq)

flipCard = FlipCard . FC
moveToFoundation = MoveToFoundation . MTF
moveStack = (MoveStack .) . MS

makePrisms ''Card
makePrisms ''Layout
makeLenses ''Foundation
makeLenses ''Game
makeLenses ''Pile
makePrisms ''Move
makeLenses ''MoveStack
makeLenses ''FlipCard
makeLenses ''MoveToFoundation

-- ToDo: encode pile count at the type level in the move type, making this function obsolete
normalize :: Int -> Move -> Move
normalize n move =
  case move of
    MoveStack (MS i j) -> MoveStack $ MS (i `mod` n) (j `mod` n)
    FlipCard (FC i) -> FlipCard $ FC (i `mod` n)
    MoveToFoundation (MTF i) -> MoveToFoundation $ MTF (i `mod` n)

randomS :: (RandomGen g, Random a) => State g a
randomS = state random

-- instances
instance Random Move where
  random = runState $ do
    let three = 3 :: Int
    choice <- (`mod` three) <$> randomS
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
