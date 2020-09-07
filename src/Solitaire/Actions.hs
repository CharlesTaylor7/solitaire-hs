{-# options_ghc -Wwarn #-}
module Solitaire.Actions where

import Solitaire.Imports
import Solitaire.Utils
import Solitaire.PrettyPrinter

import qualified Data.Vector as V
import qualified Data.IntMap as M
import qualified Data.HashSet as Set

import Data.List.NonEmpty ((<|))

import Debug.Trace

moves :: (MonadReader Config m) => m [Move]
moves = do
  numPiles <- M.size <$> view config_piles
  let
    range = [0..numPiles-1]
    moves = moveStack <$> range <*> range
    flips = flipCard <$> range
    sets = moveToFoundation <$> range
  pure $ sets ++ flips ++ moves

type MonadStack = ReaderT Config (Either InvalidMove)

nextSteps
  ::
  ( MonadReader Config m
  , MonadHistory Game m
  )
  => Game
  -> m [Step]
nextSteps game = do
  config <- ask
  history <- getHistory
  let
    paired = id &&& (\move -> runReaderT (moveReducer @MonadStack move game) config)
    step = Step ^. from curried
    unvisited = not . flip Set.member history . view step_game
    steps = runReader moves config
      ^.. folded . to paired . distributed . _Right . to step . filtered unvisited
  pure steps

scoreStep :: Step -> (Score, Score)
scoreStep (Step move game) = (scoreMove move,scoreByRuns game)
  where
    scoreMove :: Move -> Score
    scoreMove (MoveToFoundation _) = 2
    scoreMove (FlipCard _) = 1
    scoreMove (MoveStack _) = 0

newtype Run = Run (NonEmpty Card)

splitIntoRuns :: [Card] -> [Run]
splitIntoRuns cards =
  let
    reducer :: [Run] -> Card -> [Run]
    reducer [] card = [Run $ card :| []]
    reducer runs@(Run run@(c:|_) : rest) card
      | card `isSuccessorOf` c = (Run $ card <| run) : rest
      | otherwise = (Run $ card :| []) : runs
  in
    foldl' reducer [] cards

scoreRun :: Run -> Score
scoreRun (Run cards) = Score $ length cards - 1

scorePile :: PileCards -> Score
scorePile pile =
  pile ^.. faceUp . to toList . to splitIntoRuns . traverse . to scoreRun
  & sumOf folded

scoreByRuns :: Game -> Score
scoreByRuns game =
  game ^.. layout . _Layout . traverse . to scorePile
  & sumOf folded

moveReducer
  :: (MonadError InvalidMove m)
  => Move
  -> Game
  -> m Game
moveReducer move =
  case move of
    FlipCard (FC i) ->
      layout . _Layout . ix i $ \pile -> do
        if is _Just $ pile ^? faceUp . _Cons
        then throwError (CardFlipOnUnexposedPile i)
        else pure ()

        (head, rest) <- pile ^? faceDown . _Cons
          & (maybeToError $ CardFlipOnEmptyPile i)

        let faceUp' = faceUp .~ [head]
        let faceDown' = faceDown .~ rest
        pure $ pile & faceUp' . faceDown'
    MoveToFoundation (MTF i) ->
      foundation . numSets +~ 1 >>>
      (layout . _Layout . ix i $ \pile ->
        let
          (set, leftover) = pile ^. faceUp . to splitAtStack
          pile' = pile & faceUp .~ leftover
        in do
          when (length set /= enumSize @Card) $ throwError $ IncompleteSet i
          pure pile'
      )
    MoveStack (MS i j) ->
      layout . _Layout $ \layout -> do
        let
          (stack, rest) =
            layout ^?! ix i . faceUp . to splitAtStack
          target = layout ^?! ix j

--          source' = ix i . faceUp .~ rest
--          target' = ix j . faceUp %~ (stack <>)

        when (i == j) $
          throwError $ SourceIsTarget i

        when (null stack) $
          throwError $ EmptyStackSource i

        when (
            (null . view faceUp) target &&
            (not . V.null . cards) target
          ) $
          throwError $ EmptyStackTarget j

        let targetCard = layout ^?! ix j . faceUp . _head
        let stackBottomCard = stack ^?! _last
        let stackSize = length stack
        let diff = fromEnum targetCard - fromEnum stackBottomCard
        let splitStackAt = length stack + fromEnum targetCard - fromEnum stackBottomCard


--        when mismatchError $
 --         throwError $ MismatchingStacks i j
        pure layout & traceShow (stackSize, diff)
        --pure $ layout & source' . target'

isSuccessorOf :: Card -> Card -> Bool
a `isSuccessorOf` b =
  fromEnum a - fromEnum b == 1

countWhile :: Foldable f => (a -> Bool) -> f a -> Int
countWhile p =
  let
    reducer x acc
      | p x = 1 + acc
      | otherwise = acc
  in foldr reducer 0

splitAtStack :: Vector Card -> (Vector Card, Vector Card)
splitAtStack cards =
  maybe
    (mempty, mempty)
    (\(_, rest) ->
      let
        n =
          mzip rest cards
            & countWhile (uncurry isSuccessorOf)
      in V.splitAt (n+1) cards
    )
    (uncons cards)
