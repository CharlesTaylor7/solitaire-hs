{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}
module Solitaire.Core.Engine where

import Solitaire.Prelude

import Solitaire.Core.Rules
import qualified Solitaire.Core.Rules as Core

import Solitaire.Core.Config
import Solitaire.Core.Utils

import Solitaire.Core.Move.Class
import qualified Solitaire.Core.Move.Class as Move

import qualified Data.HashSet as Set

import System.Timeout
import GenericUtils (constructorName)

import qualified Data.Map as Map


data GameSolve game = Solution [game] | NoSolution
  deriving stock (Generic)


showText :: Show a => a -> Text
showText = fromString . show


heuristic
  :: forall rs m. (MonadReader Weights m, Rules rs)
  => Game rs
  -> m Float
heuristic game =
  let
    features = heuristicFeatures @rs game
  in
    reader $ sumOf
      ( ifolded
      . withIndex
      . to (\(i, w) -> features ^?! ix i . to (* w))
      )


runGame
  :: forall rs m.
  ( Solitaire rs
  , MonadReader Weights m
  , MonadIO m
  )
  => Game rs
  -> m (GameSolve (Game rs))
runGame game =
  runGameLoop @rs game
    & runPQueueT
    & runHistoryT


allMoves
  :: forall rs.
  ( Solitaire rs
  )
  => Game rs
  -> [Step (Game rs)]
allMoves game = do
  SomeMoveType (_ :: Proxy move) <- moveTypes @rs
  (uncurry Step . (_1 %~ SomeMove)) <$> Move.steps @move game


nextSteps
  :: forall rs m.
  ( Solitaire rs
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m [Step (Game rs)]

nextSteps game = do
  history <- getHistory
  let
    unvisited :: Step (Game rs) -> Bool
    unvisited = not . flip Set.member history . view #game

  pure $ allMoves @rs game & filter unvisited


runGameLoop
  :: forall rs m.
  ( Solitaire rs
  , MonadReader Weights m
  , MonadPQueue Float (GameHistory (Game rs)) m
  , MonadHistory (Game rs) m
  )
  => Game rs
  -> m (GameSolve (Game rs))

runGameLoop game = do
  priority <- heuristic @rs game
  queueInsert priority $ GameHistory 0 [game]
  conclusion <- loopM (\_ -> step @rs) ()
  pure conclusion


step
  :: forall rs m.
  ( Solitaire rs
  , MonadReader Weights m
  , MonadHistory (Game rs) m
  , MonadError (GameSolve (Game rs))  m
  , MonadPQueue Float (GameHistory (Game rs)) m
  )
  => m ()
step = do
  -- retrieve the best priority game state
  maybeMin <- queuePopMin
  case maybeMin of
    -- out of game states to try, we lost
    Nothing ->
      throwError NoSolution

    -- we have game states to play from
    Just (priority, gameHistory) -> do
      let
        game = gameHistory ^. #games . head1

      -- game states can be reached multiple times via different paths
      -- verify we haven't visted this state before
      visited <- historyHas game
      when (not visited) $ do

        -- check to see if we won
        when (gameIsWon @rs game) $
          throwError $ Solution $ gameHistory ^.. #games . folded

        -- record we visited this game state
        saveToHistory game

        -- checking the history hash set is much cheaper than spurious extra inserts to the priority queue
        -- so we check the visited set both at insert time & queue pop time
        steps <- nextSteps @rs game

        -- insert new game states reachable from this one
        ifor_ steps $ \i step -> do
          let
            moves = gameHistory ^. #moveCount . singular #_MoveCount . to fromIntegral
          h <- heuristic @rs $ step ^. #game

          queueInsert
            -- total moves made so far + estimated remaining moves
            (moves + h)
            -- update total move count & list of game states
            (gameHistory
            & #moveCount +~ 1
            & #games %~ ((step ^. #game) <|)
            )
