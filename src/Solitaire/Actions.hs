module Solitaire.Actions where

-- base
import Control.Monad.Zip
import Data.Maybe

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
  = CardFlipOnUnexposedPileError Int
  | CardFlipOnEmptyPileError Int
  deriving (Read, Show, Eq)

matches :: Traversal' s a -> s -> Bool
matches t s = isNothing $ s^? t

moveReducer :: Move -> Game -> Either InvalidMove Game
moveReducer move =
  case normalize 3 move of
    FlipCard (FC i) ->
      (layout . _Layout . ix i) $ \pile ->
      do
        _ <- if isn't (faceUp . _Empty) pile then Left (CardFlipOnEmptyPileError i) else Right ()
        let maybeFaceDown = pile ^? faceDown . _Cons
        (head, rest) <- maybeFaceDown
          & (maybeToRight $ CardFlipOnEmptyPileError i)
        let faceUp' = faceUp .~ [head]
        let faceDown' = faceDown .~ rest
        pure $ pile & faceUp' . faceDown'

    -- MoveToFoundation (MTF i) ->
    --   let
    --     updateLayout layout =
    --       let
    --         pile = layout ^?! ix i
    --         pile' = pile & faceUp %~ (view (to splitAtStack . _2))
    --       in
    --         layout
    --   in
    --     Right $
    --       (layout . _Layout %~ updateLayout)
    --       . (foundation . numSets +~ 1)

    -- MoveStack (MS i j) ->
    --   let
    --     move layout =
    --       let
    --         (stack, rest) =
    --           layout ^?! ix i . faceUp . to splitAtStack
    --         source' = ix i . faceUp .~ rest
    --         target' = ix j . faceUp <>~ stack
    --       in
    --         layout & source' . target'

    --   in Right $ layout . _Layout %~ move
