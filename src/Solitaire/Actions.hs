module Solitaire.Actions where

-- base
import Control.Monad.Zip (mzip)
import Control.Monad ((>=>))
import Control.Arrow ((>>>))
import Control.Applicative (liftA2)

import Data.Maybe
import Debug.Trace

-- lens
import Control.Lens

-- containers
import Data.IntMap (IntMap)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- either
import Data.Either.Combinators

-- app
import Solitaire.Types
import Solitaire.Utils

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

data InvalidMove
  = CardFlipOnUnexposedPile Int
  | CardFlipOnEmptyPile Int
  | IncompleteSet Int
  | MismatchingStacks Int Int
  | EmptyStackSource Int
  | EmptyStackTarget Int
  deriving (Read, Show, Eq)

errorIf :: Bool -> e -> Either e ()
errorIf b e = if b then Left e else Right ()

moveReducer :: Move -> Game -> Either InvalidMove Game
moveReducer move =
  case normalize 3 move of
    FlipCard (FC i) ->
      layout . _Layout . ix i $ \pile -> do
        _ <- if isJust $ pile ^? faceUp . _Cons
             then Left (CardFlipOnUnexposedPile i)
             else Right ()

        (head, rest) <- pile ^? faceDown . _Cons
          & (maybeToRight $ CardFlipOnEmptyPile i)

        let faceUp' = faceUp .~ [head]
        let faceDown' = faceDown .~ rest
        pure $ pile & faceUp' . faceDown'
    MoveToFoundation (MTF i) ->
      foundation . numSets +~ 1 >>>
      (layout . _Layout . ix i $ \pile ->
        let
          (set, leftover) = pile ^. faceUp . to splitAtStack
          pile' = pile & faceUp .~ leftover
        in
          if length set == setSize
          then pure pile'
          else Left $ IncompleteSet i
      )
    MoveStack (MS i j) ->
      layout . _Layout $ \layout -> do
        let
          (stack, rest) =
            layout ^?! ix i . faceUp . to splitAtStack
          source' = ix i . faceUp .~ rest
          target' = ix j . faceUp %~ (stack <>)

        errorIf (null stack) $
          EmptyStackSource i

        errorIf (isNothing $ layout ^? ix j . faceUp . _head) $
          EmptyStackTarget j

        let t = layout ^? ix j . faceUp . _head
        let s = stack ^? _last
        let match = (liftA2 isSuccessorOf) s t

        errorIf (maybe True id match) $
          MismatchingStacks i j

        pure $ layout & source' . target'
