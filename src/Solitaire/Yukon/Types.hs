module Solitaire.Yukon.Types
  ( module CoreTypes
  , Game(..)
  , Foundation(..)
  , Move(..)
  , InvalidMove(..)
  , MoveStack(..)
  , MoveToFoundation(..)
  , FlipCard(..)
  , moveStack
  , moveToFoundation
  , flipCard
  ) where

import Solitaire.Prelude
import Solitaire.Core.Types as CoreTypes
  ( Pile(..)
  , Card(..)
  , Rank(..)
  , Suit(..)
  , Color(..)
  , Config(..)
  , Tableau(..)
  )


newtype Foundation = Foundation (Map Suit Rank)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty Foundation where
  prettyExpr =
    prettyExpr .
    KeyValuePairs .
    toListOf
      ( #_Foundation
      . ifolded
      . withIndex
      . to (_1 %~ WrappedShow)
      . to (_2 %~ WrappedShow)
      )

data Game = Game
  { tableau :: Tableau Card
  , foundation :: Foundation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty Game where
  prettyExpr (Game tableau foundation) =
    PrettyHardWrap
      [ prettyExpr foundation
      , prettyExpr tableau
      ]


data InvalidMove
  = CardFlipOnUnexposedPile Int
  | CardFlipOnEmptyPile Int
  | IncompleteSet Int
  | MismatchingStacks Int Int
  | EmptyStackSource Int
  | MoveStackOntoFaceDownCards Int
  | SourceIsTarget Int
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Exception)


-- TODO: Move class
data Move
  = MoveStack MoveStack
  | FlipCard FlipCard
  | MoveToFoundation MoveToFoundation
  deriving (Eq, Show, Read, Generic)

data MoveStack = MS
  { fromIndex :: Int
  , toIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

newtype FlipCard = FC
  { pileIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

newtype MoveToFoundation = MTF
  { pileIndex :: Int
  }
  deriving (Eq, Show, Read, Generic)

flipCard :: Int -> Move
flipCard = FlipCard . FC

moveToFoundation :: Int -> Move
moveToFoundation = MoveToFoundation . MTF

moveStack :: Int -> Int -> Move
moveStack = (MoveStack .) . MS
