{-# LANGUAGE FlexibleInstances, UndecidableInstances, PartialTypeSignatures, GeneralizedNewtypeDeriving #-}
module Solitaire.PrettyPrinter where

-- base
import Control.Applicative
import Control.Arrow
import Data.Either
import Data.List

-- libraries
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lens.Micro

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

instance Pretty String where
  pretty = id

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

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc vector =
  if V.null vector
  then Nothing
  else Just $ V.init &&& V.last $ vector

peelFaceDown, peelFaceUp :: Pile -> Maybe (CardView, Pile)
peelFaceDown pile@(Pile _ vector) =
  do
    (init, last) <- unsnoc vector
    pure (FaceDown, pile { faceDown = init })

peelFaceUp pile@(Pile vector _) =
  do
    (init, last) <- unsnoc vector
    pure (FaceUp last, pile { faceUp = init })

peelCard :: Pile -> (CardView, Pile)
peelCard pile =
  maybe
    (Empty, Pile V.empty V.empty)
    id
    (peelFaceDown pile <|> peelFaceUp pile)

peelRow' :: IntMap Pile -> ([CardView], IntMap Pile)
peelRow' = traverse ((pure . fst &&& id . snd) . peelCard)

peelRow :: Layout -> (Row, Layout)
peelRow = (Row . fst &&& Layout . snd) . peelRow' . unLayout

toRows :: Layout -> [Row]
toRows = fst . loopM act
  where
    act layout =
      let
        (r@(Row row), piles) = peelRow layout
        rowWriter = ([r], piles)
      in
        if all (== Empty) row
        then pure $ Right ()
        else Left <$> rowWriter
