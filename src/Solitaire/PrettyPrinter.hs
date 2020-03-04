module Solitaire.PrettyPrinter
  ( printP
  , Pretty(..)
  ) where

import Solitaire.Imports hiding (Empty)
import Solitaire.Types
import Solitaire.Utils
import Solitaire.Invariants

import qualified Data.IntMap as M
import qualified Data.Vector as V

class Pretty a where
  pretty :: a -> String

instance Pretty InvalidMove where
  pretty = show

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = pretty x
  pretty (Right x) = pretty x

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

newtype Row = Row
  { unRow :: [CardView]
  }
  deriving (Eq, Show, Read, Semigroup, Monoid)

data CardView
  = Empty
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

toCardViews :: RowCount -> Pile -> [CardView]
toCardViews (RowCount n) =
  let
    getFaceUps = toList . fmap FaceUp . view faceUp
    getFaceDowns = toList . fmap (const FaceDown) . view faceDown
  in
    rightPad n Empty . (getFaceDowns <> getFaceUps)

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

-- exports
printP :: (MonadIO m, Pretty a) => a -> m ()
printP = liftIO . putStrLn . pretty
