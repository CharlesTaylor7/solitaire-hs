{-# OPTIONS_GHC -Wno-deprecations #-}
module Solitaire.PrettyPrinter
  ( prettyExprPrint
  , tracePretty
  , Pretty(..)
  ) where

import Solitaire.Imports hiding (Empty)
import Solitaire.Invariants

import Debug.Trace (trace)

import Data.String (IsString(..))

import qualified Data.Map as Map
import qualified Data.Text as T


pretty :: Pretty a => a -> String
pretty = evalPrettyExpr . prettyExpr

evalPrettyExpr :: PrettyExpr -> String
evalPrettyExpr = snd . runWriter . runReaderT 2 . flip evalStateT 0 . toPrettyM

type PrettyM m = (MonadState Int m, MonadState Int m, MonadWriter String m)

toPrettyM :: PrettyM m => PrettyExpr -> m ()
toPrettyM PrettyNothing =
  pure ()

toPrettyM (PrettyLine line) = do
  indentation <- flip replicate ' ' <$> get
  tell $ indentation <> line <> "\n"

toPrettyM (PrettyIndent expr) = do
  indent <- ask
  modify (+ indent)
  toPrettyM expr
  modify (subtract indent)

toPrettyM (PrettyTogether expr1 expr2) = do
  toPrettyM expr1
  toPrettyM expr2

data PrettyExpr
  = PrettyNothing
  | PrettyLine String
  | PrettyIndent PrettyExpr
  | PrettyTogether PrettyExpr PrettyExpr

instance Semigroup PrettyExpr where
  (<>) = PrettyTogether

instance Monoid PrettyExpr where
  mempty = PrettyNothing

instance IsString PrettyExpr where
  fromString = PrettyLine

class Pretty a where
  prettyExpr :: a -> PrettyExpr

instance Pretty Score where
  prettyExpr = PrettyLine . show

instance Pretty Text where
  prettyExpr = PrettyLine . T.unpack

instance Pretty a => Pretty [a] where
  prettyExpr [] = PrettyLine "[]"
  prettyExpr xs = "[" <> PrettyIndent (foldMap prettyExpr xs) <> "]"

instance Pretty a => Pretty (Vector a) where
  prettyExpr = prettyExpr . toList

instance (Pretty key, Pretty value) => Pretty (Map key value) where
  prettyExpr xs = "{" <> join xs <> "}"
    where
      format (key, value) = prettyExpr key <> ":" <> prettyExpr value
      join = foldMap format . Map.toList


instance Pretty Move where
  prettyExpr (MoveStack (MS i j)) = fromString $ "moveStack" ++ " " ++ show i ++ " " ++ show j
  prettyExpr (MoveToFoundation (MTF i)) = fromString $ "moveToFoundation" ++ " " ++ show i
  prettyExpr (FlipCard (FC i)) = fromString $ "flipCard" ++ " " ++ show i

instance Pretty InvalidMove where
  prettyExpr = show

instance Pretty Bool where
  prettyExpr = show

instance (Pretty a) => Pretty (Maybe a) where
  prettyExpr (Just x) = prettyExpr x
  prettyExpr Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyExpr (Left x) = prettyExpr x
  prettyExpr (Right x) = prettyExpr x

instance (Pretty a, Pretty b) => Pretty (a, b) where
  prettyExpr (a, b) = prettyExpr a ++ "\n" ++ prettyExpr b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  prettyExpr (a, b, c) = prettyExpr a ++ "\n" ++ prettyExpr b ++ "\n" ++ prettyExpr c

instance Pretty Char where
  prettyExpr = pure

instance Pretty Card where
  prettyExpr = show . (+ 1) . fromEnum

instance Pretty CardView where
  prettyExpr Empty = " "
  prettyExpr FaceDown = "-"
  prettyExpr (FaceUp card) = prettyExpr card

instance Pretty Row where
  prettyExpr = intercalate "|" . map prettyExpr . unRow

instance Pretty Layout where
  prettyExpr = intercalate "\n" . map prettyExpr . toRows

instance Pretty Foundation where
  prettyExpr (Foundation n) = "[" <> show n <> "]"

instance Pretty Game where
  prettyExpr (Game layout foundation) =
    prettyExpr foundation <> "\n" <> prettyExpr layout

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
toRows (Layout layout) = do
  let
    rowCount =
      layout
        & fmap pileSize
        & maximum
        & RowCount
    columns = toCardViews rowCount <$> toList layout
    rows = transpose columns & map Row

  rows

prettyExprPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyExprPrint = liftIO . putStrLn . prettyExpr

tracePretty :: Pretty a => a -> b -> b
tracePretty a = trace $ prettyExpr $ a
