{-# OPTIONS_GHC -Wno-deprecations #-}
module Solitaire.PrettyPrinter
  ( prettyPrint
  , tracePretty
  , pretty
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
evalPrettyExpr = snd . runWriter . flip runReaderT 2 . flip evalStateT 0 . toPrettyM

type PrettyM m = (MonadReader Int m, MonadState Int m, MonadWriter String m)

toPrettyM :: PrettyM m => PrettyExpr -> m ()
toPrettyM (PrettyStr text) = do
  tell text

toPrettyM (PrettyIndent expr) = do
  indent <- ask
  modify (+ indent)
  toPrettyM expr
  modify (subtract indent)

toPrettyM (PrettySoftWrap exprs) =
  for_ exprs toPrettyM

toPrettyM (PrettyHardWrap exprs) = do
  indentation <- flip replicate ' ' <$> get
  for_ exprs $ \expr -> do
    tell indentation
    toPrettyM expr
    tell "\n"

data PrettyExpr
  = PrettyStr String
  | PrettySoftWrap [PrettyExpr]
  | PrettyHardWrap [PrettyExpr]
  | PrettyIndent PrettyExpr

instance IsString PrettyExpr where
  fromString = PrettyStr

class Pretty a where
  prettyExpr :: a -> PrettyExpr

instance Pretty Score where
  prettyExpr = PrettyStr . show

instance Pretty Text where
  -- TODO: split on line breaks and use hardwrap
  prettyExpr = PrettyStr . T.unpack

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

instance Pretty InvalidMove where
  prettyExpr = PrettyStr . show

instance Pretty Bool where
  prettyExpr = PrettyStr . show

instance (Pretty a) => Pretty (Maybe a) where
  prettyExpr (Just x) = prettyExpr x
  prettyExpr Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyExpr (Left x) = prettyExpr x
  prettyExpr (Right x) = prettyExpr x

instance Pretty Char where
  prettyExpr = PrettyStr . pure

instance Pretty Card where
  prettyExpr = PrettyStr . show

instance Pretty CardView where
  prettyExpr Empty = " "
  prettyExpr FaceDown = "-"
  prettyExpr (FaceUp card) = PrettyStr . show . (+1) . fromEnum $ card

instance Pretty Row where
  prettyExpr = PrettyStr . intercalate "|" . map pretty . unRow

instance Pretty Layout where
  prettyExpr = PrettyHardWrap . map prettyExpr . toRows

instance Pretty Foundation where
  prettyExpr (Foundation n) = PrettyStr $ "[" <> show n <> "]"

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

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . putStrLn . pretty

tracePretty :: Pretty a => a -> b -> b
tracePretty = trace . pretty
