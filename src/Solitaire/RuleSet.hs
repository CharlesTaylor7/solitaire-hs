module Solitaire.RuleSet where

import Solitaire.Prelude


-- types
data Step move game = Step
  { move :: move
  , game :: game
  }
  deriving (Eq, Read, Show, Generic)

data GameWithPlayback game move = GameWithPlayback
  { game :: game
  , moves :: [move]
  }
  deriving (Generic)

newtype App game move config a = App
  { unApp :: PQueueT MoveCount (GameWithPlayback game move) (HistoryT game (ReaderT config IO)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadHistory game
    , MonadPQueue MoveCount (GameWithPlayback game move)
    , MonadReader config
    )

data GameConclusion move = GameWon [move] | GameLost
  deriving (Eq, Show)

newtype MoveCount = MoveCount Int
  deriving (Eq, Ord, Num)


class RuleSet ruleset monad where
  data Game ruleset :: *
  data Move ruleset :: *

  newGame :: monad (Game ruleset)
  gameIsWon :: Game ruleset -> Bool
