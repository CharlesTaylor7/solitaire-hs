module Solitaire.Yukon.Types
  ( module CoreTypes
  , Game
  , pattern Game
  , Foundation(..)
  , Priority(..)
  ) where

import Solitaire.Prelude
import Solitaire.Core.Types (rankToChar, suitToChar, suitColor)
import Solitaire.Core.Types as CoreTypes
  ( Pile(..)
  , Card(..)
  , Rank(..)
  , Suit(..)
  , Color(..)
  , GameConfig(..)
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
  deriving stock (Eq, Read, Show, Generic)
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
  , numFaceUp :: Int
  , numFaceDown :: Int
  , totalRunScore :: Score
  }
  deriving stock (Show, Generic)

instance Pretty Priority where
  prettyExpr p = prettyExpr $
    (mempty :: Map Text SomePretty)
    & at "order" ?~ (p & priorityOrder & SomePretty)
    & at "made" ?~ (p ^. #made . to SomePretty)
    & at "numFaceDown" ?~ (p ^. #numFaceDown . to SomePretty)
    & at "numFaceUp" ?~ (p ^. #numFaceUp . to SomePretty)
    & at "runScore" ?~ (p ^. #totalRunScore . to SomePretty)

priorityOrder :: Priority -> Float
priorityOrder p = made + estimate
  where
    made = p ^. #made . singular #_MoveCount . to fromIntegral
    numFaceUp = p ^. #numFaceUp . to fromIntegral
    numFaceDown = p ^. #numFaceDown . to fromIntegral
    runScore = p ^. #totalRunScore . singular #_Score . to fromIntegral

    estimate = weightFaceUp * numFaceUp + weightFaceDown * numFaceDown + weightRunScore * runScore
    -- face down cards count double because they have to be flipped over before being placed in the tableau

    -- weightEstimated of 1 would be A*
    -- this allows the search to reach a greater depth before backing out
    -- as such the search is not guaranteed to find the shortest solution, just a solution
    --
    weightFaceUp :: Float
    weightFaceUp = 2

    -- should be atleast twice as big as weightFaceUp
    weightFaceDown :: Float
    weightFaceDown = 5

    -- should be zero or less
    weightRunScore :: Float
    weightRunScore = -0.5


instance Eq Priority where
  (==) = (==) `on` priorityOrder

instance Ord Priority where
  compare = compare `on` priorityOrder
