module Solitaire.PrettyPrinter where

-- base
import Control.Applicative
import Control.Arrow
import Data.Either
import Data.Foldable
import Data.List

-- libraries
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Lens hiding (Empty)

-- app
import Solitaire.Types
import Solitaire.Utils

newtype Row = Row
  { unRow :: [CardView]
  }
  deriving (Eq, Show, Read, Semigroup, Monoid)

data CardView
  = Empty
  | FaceDown
  | FaceUp Card
  deriving (Eq, Read, Show)

class Pretty a where
  pretty :: a -> String

instance Pretty Char where
  pretty = pure

instance Pretty Card where
  pretty = show . (+ 1) . fromEnum

instance Pretty CardView where
  pretty Empty = " "
  pretty FaceDown = "-"
  pretty (FaceUp card) = pretty card

instance Pretty Row where
  pretty = intercalate "|" . map pretty . unRow

instance Pretty Layout where
  pretty = intercalate "\n" . map pretty . toRows

instance Pretty Foundation where
  pretty (Foundation n) = "[" <> show n <> "]"

instance Pretty Game where
  pretty (Game layout foundation) =
    pretty foundation <> "\n" <> pretty layout

rightPad :: Int -> a -> [a] -> [a]
rightPad n filler list =
  let
    k = n - length list
  in
    list <> replicate k filler

newtype RowCount = RowCount Int

toCardViews :: RowCount -> Pile -> [CardView]
toCardViews (RowCount n) =
  let
    getFaceUps = toList . fmap FaceUp . view faceUp
    getFaceDowns = toList . fmap (const FaceDown) . view faceDown
  in
    rightPad n Empty . (getFaceDowns <> getFaceUps)

pileSize :: Pile -> Int
pileSize = (+) <$> V.length . view faceUp <*> V.length . view faceDown

toRows :: Layout -> [Row]
toRows (Layout layout) =
  let
    rowCount =
      layout
        & fmap pileSize
        & maximum
        & RowCount
    columns = toCardViews rowCount <$> toList layout
    rows = transpose columns & map Row
  in rows
