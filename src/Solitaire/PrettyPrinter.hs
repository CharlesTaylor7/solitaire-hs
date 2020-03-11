module Solitaire.PrettyPrinter
  ( prettyPrint
  , tracePretty
  , Pretty(..)
  ) where

import Solitaire.Imports hiding (Empty)
import Solitaire.Utils
import Solitaire.Invariants

import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified RIO.Text as T

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty [] = "[]"
  pretty xs = "[" ++ space ++ join xs ++ "\n]"
    where
      space = '\n' : (replicate 2 ' ')
      join = intercalate space . map pretty

instance Pretty Move where
  pretty (MoveStack (MS i j)) = "moveStack" ++ " " ++ show i ++ " " ++ show j
  pretty (MoveToFoundation (MTF i)) = "moveToFoundation" ++ " " ++ show i
  pretty (FlipCard (FC i)) = "flipCard" ++ " " ++ show i

instance Pretty InvalidMove where
  pretty = show

instance Pretty Bool where
  pretty = show

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = pretty x
  pretty (Right x) = pretty x

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = pretty a ++ "\n" ++ pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = pretty a ++ "\n" ++ pretty b ++ "\n" ++ pretty c

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

toCardViews :: RowCount -> PileCards -> [CardView]
toCardViews (RowCount n) =
  let
    getFaceUps = reverse . toList . fmap FaceUp . view faceUp
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
prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . putStrLn . pretty

tracePretty :: Pretty a => a -> b -> b
tracePretty a = trace $ T.pack . pretty $ a
  where text = T.pack . pretty $ a
