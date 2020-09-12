{-# language UndecidableInstances #-}
module Solitaire.RuleSet where

import Solitaire.Prelude
import Solitaire.PrettyPrinter

import qualified Data.HashSet as Set


-- types
{--
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
deriving instance (Solitaire rs) => MonadHistory (Game rs) (App rs)
--}

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

-- catch all constraint
type Solitaire rs =
  forall config game move invalidMove.
  ( RuleSet rs config game move invalidMove
  , Eq game
  , Hashable game
  , Pretty game
  , Exception invalidMove
  )

class RuleSet rs config game move invalidMove
  | rs -> config, rs -> game, rs -> move, rs -> invalidMove
  where


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
  ( RuleSet rs
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
