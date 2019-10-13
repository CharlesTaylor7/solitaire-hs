module Solitaire.Actions where

-- base
import Control.Monad.Zip

-- lens
import Control.Lens

-- containers
import Data.IntMap (IntMap)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- app
import Solitaire.Types

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

data InvalidMove = InvalidMove
  deriving (Read, Show, Eq)

moveReducer :: Move -> Game -> Either InvalidMove Game
moveReducer move game = Right $ f game
  where
    f =
      case normalize 3 move of
        MoveStack (MS i j) ->
          let
            move layout =
              let
                (stack, rest) =
                  layout ^?! ix i . faceUp . to splitAtStack
                source' = ix i . faceUp .~ rest
                target' = ix j . faceUp <>~ stack
              in
                layout & source' . target'

          in layout . _Layout %~ move

        FlipCard (FC i) ->
          let
            flipCard pile =
              let
                (head, rest) = pile ^?! faceDown . _Cons
                faceUp' = faceUp .~ [head]
                faceDown' = faceDown .~ rest
              in
                pile & faceUp' . faceDown'
          in
            layout . _Layout . ix i %~ flipCard

        MoveToFoundation (MTF i) ->
          let
            updateLayout layout =
              let
                pile = layout ^?! ix i
                pile' = pile & faceUp %~ (view (to splitAtStack . _2))
              in
                layout
          in
            (layout . _Layout %~ updateLayout)
              . (foundation . numSets +~ 1)
