module Solitaire.Yukon.Types
  ( module CoreTypes
  , Game(..)
  , Foundation(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Types (rankToChar, suitToChar, suitColor)
import Solitaire.Core.Types as CoreTypes
  ( Pile(..)
  , Card(..)
  , Rank(..)
  , Suit(..)
  , Color(..)
  , Config(..)
  , Tableau(..)
  )

import qualified Data.DList as DL

import qualified Rainbow


newtype Foundation = Foundation (Map Suit Rank)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty Foundation where
  prettyExpr (Foundation foundation) =
    enumerate @[Suit]
      & fmap (chunkToByteStrings . formatFoundationPile)
      & DL.intercalate ["|"]
      & PrettyStr
    where
      formatFoundationPile :: Suit -> Rainbow.Chunk
      formatFoundationPile suit =
        let
          suitChar = suitToChar suit
          rankChar = foundation ^. at suit . to (fmap rankToChar) . non '_'

          textColor :: Rainbow.Radiant
          textColor =
            case suitColor suit of
              Red -> Rainbow.red
              Black -> Rainbow.black
        in
          (rankChar : suitChar : [])
          & fromString
          & Rainbow.fore textColor

chunkToByteStrings :: Rainbow.Chunk -> DList ByteString
chunkToByteStrings = fromList . chunksToByteStrings . pure

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
