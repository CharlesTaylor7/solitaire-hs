{-# language UndecidableInstances #-}
module Solitaire.RuleSet where

import Solitaire.Prelude
import Solitaire.PrettyPrinter


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
    , MonadReader (Config rs)
    )
deriving instance (Eq (Game rs), Hashable (Game rs)) => MonadHistory (Game rs) (App rs)

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


class RuleSet rs where
  data Config rs :: *
  data Game rs :: *
  data Move rs :: *
  data InvalidMove rs :: *

  newGame :: App rs (Game rs)
  gameIsWon :: Game rs -> Bool
  moveReducer :: (MonadError (InvalidMove rs) m) => Move rs -> Game rs -> m (Game rs)
  nextSteps
    ::
    ( MonadReader (Config rs) m
    , MonadHistory (Game rs) m
    )
    => Game rs
    -> m [Step rs]

type Solitaire rs =
  ( RuleSet rs
  , Eq (Game rs)
  , Hashable (Game rs)
  , Pretty (Game rs)
  , Exception (InvalidMove rs)
  )
