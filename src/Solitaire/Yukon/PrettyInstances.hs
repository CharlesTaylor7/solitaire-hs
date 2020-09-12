{-# options_ghc -Wno-orphans #-}
module Solitaire.Yukon.PrettyInstances () where

import Solitaire.Prelude

import Solitaire.Yukon.Types

import Data.String (IsString(..))

import qualified Data.Map as Map
import qualified Data.Text as T


deriving via WrappedShow Score instance Pretty Score
deriving via WrappedShow InvalidMove instance Pretty InvalidMove
deriving via WrappedShow Bool instance Pretty Bool
deriving via WrappedShow Card instance Pretty Card

instance Pretty Text where
  prettyExpr = PrettyStr

instance Pretty a => Pretty [a] where
  prettyExpr [] = "[]"
  prettyExpr xs =
    PrettyHardWrap
      [ "["
      , PrettyIndent (PrettyHardWrap $ map prettyExpr xs)
      , "]"
      ]

instance Pretty a => Pretty (Vector a) where
  prettyExpr = prettyExpr . toList

instance (Pretty key, Pretty value) => Pretty (Map key value) where
  prettyExpr xs =
    PrettyHardWrap
      [ "{"
      , PrettyIndent $ join xs
      , "}"
      ]
    where
      format (key, value) =
        PrettyHardWrap
          [ PrettySoftWrap [prettyExpr key, ":"]
          , prettyExpr value
          ]
      join = PrettyHardWrap . map format . Map.toList

instance Pretty Move where
  prettyExpr (MoveStack (MS i j)) = fromString $ "moveStack" ++ " " ++ show i ++ " " ++ show j
  prettyExpr (MoveToFoundation (MTF i)) = fromString $ "moveToFoundation" ++ " " ++ show i
  prettyExpr (FlipCard (FC i)) = fromString $ "flipCard" ++ " " ++ show i

instance (Pretty a) => Pretty (Maybe a) where
  prettyExpr (Just x) = prettyExpr x
  prettyExpr Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyExpr (Left x) = prettyExpr x
  prettyExpr (Right x) = prettyExpr x

instance Pretty Char where
  prettyExpr = PrettyStr . T.singleton

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
  deriving (Eq, Show, Read, Semigroup, Monoid)

data CardView
  = None
  | FaceDown
  | FaceUp Card
  deriving (Eq, Read, Show)

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

