module Solitaire.Types where

  import Data.IntMap (IntMap)
  import Data.Vector (Vector)

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

  data Foundation = Foundation
    { numSets :: Int
    }
    deriving (Eq, Show, Read)

  data Game = Game
    { layout :: IntMap Pile
    , foundation :: Foundation
    }
    deriving (Eq, Show, Read)

  data Move
    = MoveStack MoveStack
    | MoveToFoundation MoveToFoundation
    | FlipCard FlipCard
    deriving (Eq, Show, Read)

  data FlipCard = FC
    { fc_pileIndex :: Int
    }
    deriving (Eq, Show, Read)

  data MoveToFoundation = MTF
    { mtf_pileIndex :: Int
    }
    deriving (Eq, Show, Read)

  data MoveStack = MS
    { ms_pileIndex1 :: Int
    , ms_pileIndex2 :: Int
    }
    deriving (Eq, Show, Read)
