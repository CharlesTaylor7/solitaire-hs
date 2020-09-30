module Solitaire.Yukon.Types
  ( module CoreTypes
  , Game
  , pattern Game
  , Foundation(..)
  , Priority(..)
  , Estimated(..)
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
  , Score(..)
  , MoveCount(..)
  )
import qualified Solitaire.Core.Types as Core

import qualified Data.DList as DL

import qualified Rainbow


type Game = Core.Game Card Foundation ()

pattern Game :: Tableau Card -> Foundation -> Game
pattern Game tableau foundation = Core.Game tableau foundation ()


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

data Priority = Priority
  { made :: MoveCount
  , estimated :: Estimated
  , numFaceDown :: Int
  , totalRunScore :: Score
  }
  deriving stock (Show, Generic)

newtype Estimated = Estimated MoveCount
  deriving stock (Show, Eq, Ord, Generic)

priorityOrder :: Priority -> Float
priorityOrder p = made + weightEstimated * estimated
  where
    made = p ^. #made . singular #_MoveCount . to fromIntegral
    estimated = p ^. #estimated . singular(#_Estimated. #_MoveCount) . to fromIntegral

    -- weightEstimated of 1 would be A*
    -- this allows the search to reach a greater depth before backing out
    -- as such the search is not guaranteed to find the shortest solution, just a solution
    weightEstimated :: Float
    weightEstimated = 2


instance Eq Priority where
  (==) = (==) `on` priorityOrder

instance Ord Priority where
  compare = compare `on` priorityOrder
