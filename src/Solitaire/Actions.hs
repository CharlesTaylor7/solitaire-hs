{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Solitaire.Actions where

import Solitaire.Imports
import Solitaire.PrettyPrinter
import Solitaire.Types
import Solitaire.Utils
import qualified Data.Vector as V

numPiles :: Int
numPiles = 3

moves :: [Move]
moves = moves ++ flips ++ sets
  where
    range = [0..numPiles-1]
    moves = moveStack <$> range <*> range
    flips = flipCard <$> range
    sets = moveToFoundation <$> range

distributed :: Iso' (a, Either b c) (Either (a, b) (a, c))
distributed = iso to from
  where
    to (a, Left b) = Left (a, b)
    to (a, Right c) = Right (a, c)
    from (Left (a, b)) = (a, Left b)
    from (Right (a, c)) = (a, Right c)

type Error = Either InvalidMove

validSteps :: Game -> [Step]
validSteps g =
  let
    paired = id &&& flip (moveReducer @Error) g
    step = Step ^. from curried
  in
    moves ^.. folded . to paired . distributed . _Right . to step

moveReducer
  :: (MonadError InvalidMove m)
  => Move
  -> Game
  -> m Game
moveReducer move =
  case normalize numPiles move of
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

        ifThenError (i == j)
          $ SourceIsTarget i

        ifThenError (null stack)
          $ EmptyStackSource i
        ifThenError (isNothing $ layout ^? ix j . faceUp . _head)
          $ EmptyStackTarget j

        let t = layout ^?! ix j . faceUp . _head
        let s = stack ^?! _last
        let match = t `isSuccessorOf` s
        ifThenError (not match)
          $ MismatchingStacks i j

        pure $ layout & source' . target'

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
