{-# options_ghc -Wno-orphans #-}
module Solitaire.Boring.Types
  ( module CoreTypes
  , Card(..)
  , Config(..)
  , Game(..)
  , Foundation(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Types as CoreTypes
  ( Pile(..)
  , Score(..)
  , Config(..)
  , Tableau(..)
  )

import Solitaire.Core.Card (IsCard(..))


data Card
  = One
  | Two
  | Three
  | Four
  | Five
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance IsCard Card where
  a `isSuccessorOf` b =
    fromEnum a - fromEnum b == 1

instance Pretty Card where
  prettyExpr = fromString . show . (+1) . fromEnum

instance PrettyCard Card where
  prettyWidth = 1


newtype Foundation = Foundation
  { numSets :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty Foundation where
  prettyExpr (Foundation n) = PrettyStr
    [ "["
    , fromString (show n)
    , "]"
    ]


data Game = Game
  { tableau :: Tableau Card
  , foundation :: Foundation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty Game where
  prettyExpr (Game layout foundation) =
    PrettyHardWrap
      [ prettyExpr foundation
      , prettyExpr layout
      ]
