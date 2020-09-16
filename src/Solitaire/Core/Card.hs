module Solitaire.Core.Card
  ( IsCard(..)
  , Run(..)
  , splitAtFirstRun
  , splitIntoRuns
  ) where

import Solitaire.Prelude

import qualified Data.Vector as V

class IsCard card where
  isSuccessorOf :: card -> card -> Bool


splitAtFirstRun :: IsCard card => Vector card -> (Vector card, Vector card)
splitAtFirstRun cards =
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


countWhile :: Foldable f => (a -> Bool) -> f a -> Int
countWhile p =
  let
    reducer x acc
      | p x = 1 + acc
      | otherwise = acc
  in foldr reducer 0


newtype Run card = Run (NonEmpty card)
  deriving stock (Generic)
  deriving newtype (Foldable)


splitIntoRuns :: forall card. IsCard card => [card] -> [Run card]
splitIntoRuns cards =
  let
    reducer :: [Run card] -> card -> [Run card]
    reducer [] card = [Run $ card :| []]
    reducer runs@(Run run@(c:|_) : rest) card
      | card `isSuccessorOf` c = (Run $ card <| run) : rest
      | otherwise = (Run $ card :| []) : runs
  in
    foldl' reducer [] cards
