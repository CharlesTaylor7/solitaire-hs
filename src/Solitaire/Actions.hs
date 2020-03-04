{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Solitaire.Actions where

import Solitaire.Imports
import Solitaire.Types
import Solitaire.Utils
import qualified Data.Vector as V

setSize :: Int
setSize = enumSize @Card

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

ifThenError :: (MonadError e m) => Bool -> e -> m ()
ifThenError True  e = throwError e
ifThenError False _ = pure ()

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError e Nothing = throwError e
maybeToError _ (Just a) = pure a

moveReducer
  :: (MonadError InvalidMove m)
  => Move
  -> Game
  -> m Game
moveReducer move =
  case normalize 3 move of
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
          ifThenError (length set /= setSize) $ IncompleteSet i
          pure pile'
      )
    MoveStack (MS i j) ->
      layout . _Layout $ \layout -> do
        let
          (stack, rest) =
            layout ^?! ix i . faceUp . to splitAtStack
          source' = ix i . faceUp .~ rest
          target' = ix j . faceUp %~ (stack <>)

        ifThenError (null stack)
          $ EmptyStackSource i
        ifThenError (isNothing $ layout ^? ix j . faceUp . _head)
          $ EmptyStackTarget j

        let t = layout ^? ix j . faceUp . _head
        let s = stack ^? _last
        let match = (liftA2 isSuccessorOf) s t

        ifThenError (maybe True id match)
          $ MismatchingStacks i j

        pure $ layout & source' . target'
