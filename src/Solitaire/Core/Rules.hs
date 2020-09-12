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
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadPQueue MoveCount (GameWithPlayback rs)
    )


newtype MoveCount = MoveCount Int
  deriving (Eq, Ord, Num)

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
  type Config      rs = (config :: *)      | config -> rs
  type Game        rs = (game :: *)        | game -> rs
  type Move        rs = (move :: *)        | move -> rs
  type InvalidMove rs = (invalidMove :: *) | invalidMove -> rs
  type Card        rs = (card :: *)        | card -> rs

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
  ::
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
    paired = id &&& (\move -> runReaderT (moveReducer move game) config)
    step = Step ^. from curried
    unvisited = not . flip Set.member history . view #game
    steps = runReader moves config
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited
  pure steps
