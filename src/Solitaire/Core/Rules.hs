{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DataKinds #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude
import Solitaire.Core.Config

import Solitaire.Core.Move.Class
import Solitaire.Core.Types (MoveCount, Tableau)
import qualified Solitaire.Core.Types as Core

import qualified Data.HashSet as Set


type Game rs = Core.Game (Card rs) (Foundation rs) (Stock rs)


class Rules rs where
  type Config (rs :: *) :: *
  type Card (rs :: *) :: *
  type Stock (rs :: *) :: *
  type Foundation (rs :: *) :: *
  type Priority (rs :: *) :: *

  type Stock rs = ()

  moveTypes :: [SomeMoveType (Game rs)]

  -- TODO:
  -- should be MonadPrimitive + MonadRandom instead of MonadIO
  -- IO is used for mutable vectors & randomness to shuffle the deck
  -- We don't need full IO
  newGame :: (MonadIO m, MonadReader (Config rs) m) => m (Game rs)

  gameIsWon :: Game rs -> Bool

  heuristic :: Game rs -> MoveCount -> Priority rs


-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , IsConfig (Config rs)
  , PrettyCard (Card rs), Eq (Card rs), Hashable (Card rs), Ord (Card rs)
  , Pretty (Foundation rs), Eq (Foundation rs), Hashable (Foundation rs)
  , Eq (Stock rs), Hashable (Stock rs)
  , Ord (Priority rs), Show (Priority rs)
  )


-- data types
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

data GameHistory game = GameHistory
  { moveCount :: MoveCount
  , games :: NonEmpty game
  }
  deriving stock (Generic)


newtype App config game priority a = App
  { unApp :: PQueueT priority (GameHistory game) (HistoryT game (ReaderT config IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadPQueue priority (GameHistory game)
    , MonadReader config
    , MonadHistory game
    )
