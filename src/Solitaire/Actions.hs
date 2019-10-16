module Solitaire.Actions where

-- base
import Control.Monad.Zip (mzip)
import Control.Monad ((>=>))
import Control.Arrow ((>>>))
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
  = CardFlipOnUnexposedPileError Int
  | CardFlipOnEmptyPileError Int
  | IncompleteSetError Int
  deriving (Read, Show, Eq)

moveReducer :: Move -> Game -> Either InvalidMove Game
moveReducer move =
  case normalize 3 move of
    FlipCard (FC i) ->
      layout . _Layout . ix i $ \pile -> do
        _ <- if isJust $ pile ^? faceUp . _Cons
             then Left (CardFlipOnUnexposedPileError i)
             else Right ()

        (head, rest) <- pile ^? faceDown . _Cons
          & (maybeToRight $ CardFlipOnEmptyPileError i)

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
          else Left $ IncompleteSetError i
      )

    MoveStack (MS i j) ->
      (layout . _Layout $ \layout ->
        let
          (stack, rest) =
            layout ^?! ix i . faceUp . to splitAtStack
          source' = ix i . faceUp .~ rest
          target' = ix j . faceUp <>~ stack
        in
          pure $ layout & source' . target')
