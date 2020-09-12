{-# language UndecidableInstances #-}
module Solitaire.RuleSet where

import Solitaire.Prelude


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
  { game :: Game rs
  , moves :: [Move rs]
  }
  deriving (Generic)


class RuleSet rs where
  data Config rs :: *
  data Game rs :: *
  data Move rs :: *
  data InvalidMove rs :: *

  newGame :: App rs (Game rs)
  gameIsWon :: Game rs -> Bool
