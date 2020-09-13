{-# options_ghc -Wwarn #-}
module Solitaire.Yukon.PrettyInstances () where

import Solitaire.Prelude

import Solitaire.Yukon.Types


import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Rainbow
-- import qualified Data.DList as DL

deriving via WrappedShow InvalidMove instance Pretty InvalidMove

instance Pretty Move where
  prettyExpr (MoveStack (MS i j)) = fromString $ "moveStack" ++ " " ++ show i ++ " " ++ show j
  prettyExpr (MoveToFoundation (MTF i)) = fromString $ "moveToFoundation" ++ " " ++ show i
  prettyExpr (FlipCard (FC i)) = fromString $ "flipCard" ++ " " ++ show i

