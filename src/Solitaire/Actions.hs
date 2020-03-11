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
  pure $ moves ++ flips ++ sets

type MonadStack = ReaderT Config (Either InvalidMove)

validSteps :: MonadReader Config m => Game -> m [Step]
validSteps game = do
  config <- ask
  let
    ms = runReader moves config
    paired = id &&& (\move -> runReaderT (moveReducer @MonadStack move game) config)
    step = Step ^. from curried
  pure $ ms ^.. folded . to paired . distributed . _Right . to step

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
isSuccessorOf a b =
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
