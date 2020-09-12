{-# options_ghc -Wno-orphans #-}
module Solitaire.Boring.PrettyInstances () where

import Solitaire.Prelude

import Solitaire.Boring.Types

import Data.String (IsString(..))

import qualified Data.Text as T


deriving via WrappedShow InvalidMove instance Pretty InvalidMove
deriving via WrappedShow Card instance Pretty Card

instance Pretty Move where
  prettyExpr (MoveStack (MS i j)) = fromString $ "moveStack" ++ " " ++ show i ++ " " ++ show j
  prettyExpr (MoveToFoundation (MTF i)) = fromString $ "moveToFoundation" ++ " " ++ show i
  prettyExpr (FlipCard (FC i)) = fromString $ "flipCard" ++ " " ++ show i

instance Pretty CardView where
  prettyExpr None = " "
  prettyExpr FaceDown = "-"
  prettyExpr (FaceUp card) = PrettyStr . T.pack . show . (+1) . fromEnum $ card

instance Pretty Row where
  prettyExpr = PrettyStr . T.intercalate "|" . map pretty . unRow

instance Pretty Layout where
  prettyExpr = PrettyHardWrap . map prettyExpr . toRows

instance Pretty Foundation where
  prettyExpr (Foundation n) = PrettyStr $ "[" <> T.pack (show n) <> "]"

instance Pretty Game where
  prettyExpr (Game layout foundation) =
    PrettyHardWrap
      [ prettyExpr foundation
      , prettyExpr layout
      ]

newtype Row = Row
  { unRow :: [CardView]
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

data CardView
  = None
  | FaceDown
  | FaceUp Card
  deriving (Eq, Show)

newtype RowCount = RowCount Int

rightPad :: Int -> a -> [a] -> [a]
rightPad n filler list =
  let
    k = n - length list
  in
    list <> replicate k filler

toCardViews :: RowCount -> PileCards -> [CardView]
toCardViews (RowCount n) =
  let
    getFaceUps = reverse . toList . fmap FaceUp . view #faceUp
    getFaceDowns = toList . fmap (const FaceDown) . view #faceDown
  in
    rightPad n None . (getFaceDowns <> getFaceUps)

toRows :: Layout -> [Row]
toRows (Layout layout) = do
  let
    rowCount =
      layout
        & fmap pileSize
        & maximum
        & RowCount
    columns = toCardViews rowCount <$> toList layout
    rows = transpose columns & map Row

    pileSize :: PileCards -> Int
    pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)

  rows
