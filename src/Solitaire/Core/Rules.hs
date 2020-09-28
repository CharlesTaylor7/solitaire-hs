{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DataKinds #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude
import Solitaire.Core.Config

import Solitaire.Core.Move.Class

import qualified Data.HashSet as Set


class Rules rs where
  type Config (rs :: *) :: *
  type Game (rs :: *) :: *


  moveTypes :: [SomeMoveType (Game rs)]

  -- TODO:
  -- should be MonadPrimitive + MonadRandom instead of MonadIO
  -- IO is used for mutable vectors & randomness to shuffle the deck
  -- We don't need full IO
  newGame :: (MonadIO m, MonadReader (Config rs) m) => m (Game rs)

  gameIsWon :: Game rs -> Bool


-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , Eq (Game rs)
  , Hashable (Game rs)
  , Pretty (Game rs)
  , IsConfig (Config rs)
  )


data Step game = Step
  { move :: SomeMove game
  , game :: game
  }
  deriving (Generic)

instance Pretty game => Pretty (Step game) where
  prettyExpr (Step move game) =
    PrettyHardWrap
      [ prettyExpr move
      , prettyExpr game
      ]

-- data types
newtype App config game a = App
  { unApp :: PQueueT MoveCount (NonEmpty game) (HistoryT game (ReaderT config IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadPQueue MoveCount (NonEmpty game)
    , MonadReader config
    , MonadHistory game
    )


newtype MoveCount = MoveCount Int
  deriving stock (Eq, Ord)
  deriving newtype (Num)
