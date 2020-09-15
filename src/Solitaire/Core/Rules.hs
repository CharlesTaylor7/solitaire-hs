{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# options_ghc -Wwarn #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude
import Solitaire.Core.Config

import Solitaire.Core.Move (IsMove, IsMoveConstraints, InvalidMove)
import qualified Solitaire.Core.Move as Move

import qualified Data.HashSet as Set

import Control.Exception (SomeException(..))
import Data.Proxy
import GHC.OverloadedLabels (IsLabel(..))
import GHC.Records (HasField(..))


data SomeMoveType game =
  forall move. IsMoveConstraints move game =>
    SomeMoveType (Proxy move)

data SomeMove game =
  forall move. IsMoveConstraints move game =>
    SomeMove move


applySomeMove :: (MonadError SomeException m) => SomeMove game -> game -> m game
applySomeMove (SomeMove move) =
  liftEither .
  over _Left SomeException .
  Move.apply move


allMoves
  :: forall rs config game.
  ( Rules rs
  , IsConfig config
  , Game rs ~ game
  )
  => config
  -> [SomeMove game]
allMoves config =
  let
    pileCount :: NumPiles
    pileCount = numPiles config

  in
    moveTypes @rs
      >>= \(SomeMoveType (proxy :: Proxy move)) ->
        let
          oldMoves :: [ move]
          oldMoves = Move.moves @move pileCount

          moves :: [SomeMove game]
          moves = SomeMove <$> oldMoves
        in
          moves






-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , Eq (Game rs)
  , Hashable (Game rs)
  , Pretty (Game rs)
  , IsConfig (Config rs)
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


nextSteps
  :: forall rs m.
  ( Solitaire rs
  , MonadReader (Config rs) m
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m [Step (Game rs)]

nextSteps game = do
  config <- ask
  history <- getHistory
  let
    paired = id &&& (\move -> runReaderT (applySomeMove move game) config)
    step = Step ^. from curried
    unvisited = not . flip Set.member history . view #game

    someMoves :: [SomeMove (Game rs)]
    someMoves = allMoves @rs config

    steps :: [Step (Game rs)]
    steps = someMoves
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited

  pure steps
