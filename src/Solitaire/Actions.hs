{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Solitaire.Actions where

import Solitaire.Imports
import Solitaire.PrettyPrinter
import Solitaire.Utils
import qualified Data.Vector as V
import qualified Data.IntMap as M

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

validSteps :: MonadReader Config m => Game -> m [Step]
validSteps game = do
  config <- ask
  let
    paired = id &&& (\move -> runReaderT (moveReducer @MonadStack move game) config)
    step = Step ^. from curried
    steps = runReader moves config
      ^.. folded . to paired . distributed . _Right . to step
      & sortOn (Down . scoreStep)
  pure steps

scoreStep :: Step -> (Score, Score)
scoreStep (Step move game) = (scoreMove move,scoreByRuns game)
  where
    scoreMove :: Move -> Score
    scoreMove (MoveToFoundation _) = 2
    scoreMove (FlipCard _) = 1
    scoreMove (MoveStack _) = 0

newtype Run = Run [Card]

data Accumulator = Acc
  { _current_run :: ![Card]
  , _runs :: ![Run]
  }



splitIntoRuns :: [Card] -> [Run]
splitIntoRuns cards =
  let
    reducer :: Accumulator -> Card -> Accumulator
    reducer (Acc [] rs) card = Acc [card] rs
    reducer (Acc run@(c:_) rs) card
      | card `isSuccessorOf` c = Acc (card:run) rs
      | otherwise = Acc [card] (Run run : rs)
    Acc run rs = foldl' reducer (Acc [] []) cards
  in
    Run run : rs

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
        if isJust $ pile ^? faceUp . _Cons
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
          ifThenError (length set /= enumSize @Card) $ IncompleteSet i
          pure pile'
      )
    MoveStack (MS i j) ->
      layout . _Layout $ \layout -> do
        let
          (stack, rest) =
            layout ^?! ix i . faceUp . to splitAtStack
          target = layout ^?! ix j
          target_faceUp = target ^. faceUp
          target_faceDown = target ^. faceDown

          source' = ix i . faceUp .~ rest
          target' = ix j . faceUp %~ (stack <>)

        ifThenError (i == j)
          $ SourceIsTarget i

        ifThenError (null stack)
          $ EmptyStackSource i
        ifThenError (
          (null . view faceUp) target &&
          (not . V.null . cards) target )
          $ EmptyStackTarget j

        let t = layout ^? ix j . faceUp . _head
        let s = stack ^?! _last
        let errorCondition = maybe False (not . flip isSuccessorOf s) t
        ifThenError errorCondition
          $ MismatchingStacks i j

        pure $ layout & source' . target'

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
    (\(head, rest) ->
      let
        n =
          mzip rest cards
            & countWhile (uncurry isSuccessorOf)
      in V.splitAt (n+1) cards
    )
    (uncons cards)
