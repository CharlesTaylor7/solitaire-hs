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
  pretty = (: [])

instance Pretty String where
  pretty = id

instance Pretty Card where
  pretty = show . (+ 1) . fromEnum

faceDownCard :: String
faceDownCard = "-"

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc vector =
  if V.null vector
  then Nothing
  else Just $ V.init &&& V.last $ vector

printFaceDown, printFaceUp :: Pile -> Maybe (String, Pile)
printFaceDown pile@(Pile _ vector) =
  do
    (init, last) <- unsnoc vector
    pure ("-", pile { faceDown = init })

printFaceUp pile@(Pile vector _) =
  do
    (init, last) <- unsnoc vector
    pure (pretty last, pile { faceUp = init })

printCard :: Pile -> (String, Pile)
printCard pile =
  maybe
    (" ", Pile V.empty V.empty)
    id
    (printFaceDown pile <|> printFaceUp pile)

printRow :: IntMap Pile -> (String, IntMap Pile)
printRow = traverse printCard

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
