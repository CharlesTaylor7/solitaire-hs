{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
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

class Pretty a where
  pretty :: a -> String

instance {-# INCOHERENT #-} Show a => Pretty a where
  pretty = show

instance Pretty Char where
  pretty = pure

instance Pretty String where
  pretty = id

instance Pretty Card where
  pretty = show . (+ 1) . fromEnum

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

printRow :: IntMap Pile -> ([CardView], IntMap Pile)
printRow = traverse ((pure . fst &&& id . snd) . peelCard)

data CardView
  = Empty
  | FaceDown
  | FaceUp Card

instance Pretty CardView where
  pretty Empty = " "
  pretty FaceDown = "-"
  pretty (FaceUp card) = pretty card

type Row = IntMap CardView

toRows :: IntMap Pile -> [Row]
toRows piles =

instance Pretty Game where
  pretty (Game layout _) = intercalate "|" rows
    where
      (rows, _) = loopM act layout
      act layout =
        let
          (row, piles) = printRow layout
        in
          if all (== ' ') row
          then pure $ Right ()
          else pure $ Left piles

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = act x >>= loopM act ||| pure
