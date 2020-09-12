{-# language UndecidableInstances #-}
module Solitaire.Core.Rules where

import Solitaire.Prelude

import qualified Data.HashSet as Set


-- catch all constraint
type Solitaire rs =
  ( Rules rs
  , Eq (Game rs)
  , Hashable (Game rs)
  , Pretty (Game rs)
  , Exception (InvalidMove rs)
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

data GameConclusion rs = GameWon [Move rs] | GameLost
deriving instance Show (Move rs) => Show (GameConclusion rs)

data Step rs = Step
  { move :: Move rs
  , game :: Game rs
  }
  deriving (Generic)

data GameWithPlayback rs = GameWithPlayback
  { moves :: [Move rs]
  , game :: Game rs
  }
  deriving (Generic)


class Rules rs where
  type Config rs :: *
  type Game rs :: *
  type Move rs :: *
  type InvalidMove rs :: *

  newGame :: (MonadIO m, MonadReader (Config rs) m) => m (Game rs)

  gameIsWon :: Game rs -> Bool

  moves :: (MonadReader (Config rs) m) => m [Move rs]

  moveReducer
    :: forall m.
    ( MonadError (InvalidMove rs) m
    )
    => Move rs
    -> Game rs
    -> m (Game rs)


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
    steps = runReader (moves @rs) config
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited
  pure steps
