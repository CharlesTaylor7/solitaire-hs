{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Solitaire.Types where

import Data.IntMap (IntMap)
import Data.Vector (Vector)
import Control.Monad.Trans.State.Strict
import Lens.Micro
import System.Random(Random(..), RandomGen)

data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Read, Ord, Enum)

data Pile = Pile
  { faceUp :: Vector Card
  , faceDown :: Vector Card
  }
  deriving (Eq, Show, Read)

newtype Layout = Layout
  { unLayout :: IntMap Pile
  }
  deriving (Eq, Show, Read)

data Foundation = Foundation
  { numSets :: Int
  }
  deriving (Eq, Show, Read)

data Game = Game
  { layout :: Layout
  , foundation :: Foundation
  }
  deriving (Eq, Show, Read)

data Move
  = MoveStack MoveStack
  | FlipCard FlipCard
  | MoveToFoundation MoveToFoundation
  deriving (Eq, Show, Read)

data MoveStack = MS
  { ms_fromIndex :: Int
  , ms_toIndex :: Int
  }
  deriving (Eq, Show, Read)

data FlipCard = FC
  { fc_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

data MoveToFoundation = MTF
  { mtf_pileIndex :: Int
  }
  deriving (Eq, Show, Read)

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
