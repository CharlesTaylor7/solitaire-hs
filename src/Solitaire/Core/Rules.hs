{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DataKinds #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude
import Solitaire.Core.Move

import qualified Data.HashSet as Set

import Data.Proxy
import GHC.OverloadedLabels (IsLabel(..))


data SomeMoveType game = SomeMoveType
  (forall move. IsMove move game => Proxy move)

data SomeMove game = SomeMove
  (forall move. IsMove move game => move)


-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , Eq (Game rs)
  , Hashable (Game rs)
  , Pretty (Game rs)
  , MonadReader (Config rs) (App rs)
  , MonadHistory (Game rs) (App rs)
  )

-- types
newtype App rs a = App
  { unApp :: PQueueT MoveCount (GameWithPlayback rs) (HistoryT (Game rs) (ReaderT (Config rs) IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadPQueue MoveCount (GameWithPlayback rs)
    )


newtype MoveCount = MoveCount Int
  deriving stock (Eq, Ord)
  deriving newtype (Num)

data GameConclusion game = GameWon [SomeMove game] | GameLost

data Step game = Step
  { move :: SomeMove game
  , game :: game
  }
  deriving (Generic)

data GameWithPlayback game = GameWithPlayback
  { moves :: [SomeMove game]
  , game :: game
  }
  deriving (Generic)


class Rules rs where
  type Config rs :: *
  type Game rs :: *

  moveTypes :: [SomeMoveType (Game rs)]

  -- TODO:
  -- should be MonadPrimitive + MonadRandom instead of MonadIO
  -- IO is used for mutable vectors & randomness to shuffle the deck
  -- We don't need full IO
  newGame :: (MonadIO m, MonadReader (Config rs) m) => m (Game rs)

  gameIsWon :: Game rs -> Bool

allMoves :: forall rs m. (Rules rs, MonadReader (Config rs) m) => m [SomeMove (Game rs)]
allMoves = reader $ \config -> do
  SomeMoveType tmoveType <- moveTypes
  undefined
  --moves



nextSteps
  :: forall rs m.
  ( Rules rs
  , MonadReader (Config rs) m
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m [Step rs]

nextSteps game = do
  config <- ask
  history <- getHistory
  let
    paired = id &&& (\move -> runReaderT (moveReducer @rs move game) config)
    step = Step ^. from curried
    unvisited = not . flip Set.member history . view #game
    steps = runReader (allMoves @rs) config
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited
  pure steps
