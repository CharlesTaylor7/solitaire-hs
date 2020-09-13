{-# options_ghc -Wno-orphans #-}
module Solitaire.Boring.PrettyInstances () where

import Solitaire.Prelude

import Solitaire.Boring.Types


deriving via WrappedShow InvalidMove instance Pretty InvalidMove
deriving via WrappedShow Card instance Pretty Card


instance Pretty Move where
  prettyExpr (MoveStack (MS i j)) = fromString $ "moveStack" ++ " " ++ show i ++ " " ++ show j
  prettyExpr (MoveToFoundation (MTF i)) = fromString $ "moveToFoundation" ++ " " ++ show i
  prettyExpr (FlipCard (FC i)) = fromString $ "flipCard" ++ " " ++ show i


instance Pretty Foundation where
  prettyExpr (Foundation n) = PrettyStr
    [ "["
    , fromString (show n)
    , "]"
    ]


instance Pretty Game where
  prettyExpr (Game layout foundation) =
    PrettyHardWrap
      [ prettyExpr foundation
      , prettyExpr layout
      ]
