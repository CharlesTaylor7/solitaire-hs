{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DataKinds #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude
import Solitaire.Core.Config

import Solitaire.Core.Move.Class
import Solitaire.Core.Types (MoveCount, Tableau, GameConfig)
import qualified Solitaire.Core.Types as Core

import qualified Data.HashSet as Set


type Game rs = Core.Game (Card rs) (Foundation rs) (Stock rs)


class Rules rs where
  type Card rs
  type Stock rs
  type Foundation rs

  type Stock rs = ()

  moveTypes :: [SomeMoveType (Game rs)]

  -- TODO:
  -- should be MonadPrimitive + MonadRandom instead of MonadIO
  -- IO is used for mutable vectors & randomness to shuffle the deck
  -- We don't need full IO
  newGame :: (MonadIO m, MonadReader GameConfig m) => m (Game rs)

  gameIsWon :: Game rs -> Bool

  heuristicFeatures :: Game rs -> Weights

type CardConstraints card =
  ( PrettyCard card
  , Ord card
  , Hashable card
  , Bounded card
  , Enum card
  , Read card
  , Show card
  )

-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , CardConstraints (Card rs)
  , Pretty (Foundation rs), Eq (Foundation rs), Hashable (Foundation rs)
  , Eq (Stock rs), Hashable (Stock rs), Show (Stock rs), Read (Stock rs)
  )


-- data types
type Weights = Map Text Weight
type Weight = Float


data AppConfig = AppConfig
  { stats :: StatsConfig
  , game :: GameConfig
  }
  deriving stock (Generic)

data StatsConfig = StatsConfig
  { numTrials :: Int
  , microSecondsTimeout :: Int
  , heuristicWeights :: Weights
  }
  deriving stock (Generic)


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
