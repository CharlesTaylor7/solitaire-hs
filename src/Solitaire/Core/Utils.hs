module Solitaire.Core.Utils where

import Solitaire.Prelude
import Solitaire.Core.Types
import Solitaire.Core.Card

import qualified Data.Map as Map
import qualified Data.Set as Set


color :: Suit -> Color
color Hearts = Red
color Diamonds = Red
color _ = Black


pileCountsSize :: Pile Int -> Int
pileCountsSize counts = counts ^. #faceUp + counts ^. #faceDown


cards :: Semigroup a => Pile a -> a
cards pile = pile ^. #faceUp <> pile ^. #faceDown


emptyPile :: Monoid a => Pile a
emptyPile = Pile mempty mempty


getDeck :: (Enum card, Bounded card) => MonadReader Config m => m [card]
getDeck = do
  numSets <- view #numSets
  pure $ enumerate >>= replicate numSets


toPile :: (IsList l, Item l ~ card) => [card] -> Pile Int -> Pile l
toPile cards counts =
  let
    n = counts ^. #faceUp
    (up, down) = splitAt n cards
  in
    Pile (fromList up) (fromList down)


pileSize :: Foldable f => Pile (f card) -> Int
pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)


totalCards :: Tableau card -> Int
totalCards = sumOf (#_Tableau . folded . to pileSize)


cardsRemaining' :: Game c f s -> MoveCount
cardsRemaining' = view
  $ #tableau
  . #_Tableau
  . folded -- piles
  . folded -- vectors
  . to (MoveCount . length)


-- TODO: revert this
cardsRemaining :: Game c f s -> MoveCount
cardsRemaining game =
  case cardsRemaining' game of
    mc@(MoveCount n)
      | n > 52 -> error $ "estimated move count exceeded 52"
      | otherwise -> mc

scoreByRuns :: IsCard card => Tableau card -> Score
scoreByRuns game =
  game & sumOf (#_Tableau . folded . to scorePile)

scorePile :: (IsCard card, Foldable f) => Pile (f card) -> Score
scorePile pile =
  pile & sumOf (#faceUp . to toList . to splitIntoRuns . traverse . to scoreRun)

scoreRun :: Run card -> Score
scoreRun (Run cards) = Score $ length cards - 1

newtype PileId = PileId Int
  deriving stock (Eq, Show)
  deriving anyclass (Pretty)

newtype StackId = StackId Int
  deriving stock (Eq, Show)
  deriving anyclass (Pretty)

indexedCards :: forall card. IndexedTraversal' (PileId, IsFaceUp, StackId) (Tableau card) card
indexedCards =
  (indexPiles <.> indexIsFaceUp <.> indexStack)
  & reindexed (\(pileId, (isFaceUp, stackId)) -> (pileId, isFaceUp, stackId))
  where
    indexPiles :: IndexedTraversal' PileId (Tableau card) (Pile (Vector card))
    indexPiles = #_Tableau . itraversed & reindexed PileId

    indexIsFaceUp :: IndexedTraversal' IsFaceUp (Pile a) a
    indexIsFaceUp = itraversed

    indexStack :: IndexedTraversal' StackId (Vector a) a
    indexStack = itraversed & reindexed StackId

type EndoEndo a = Endo (a -> a)

toReverseMapOf :: forall a i s. (Eq i, Ord a) => IndexedGetting i (EndoEndo (Map a [i])) s a -> s -> Map a [i]
toReverseMapOf fold = ifoldlOf' fold reducer mempty
  where
    reducer :: i -> Map a [i] -> a -> Map a [i]
    reducer i map a = map & at a . non mempty %~ cons i


duplicates :: forall a i. Map a [i] -> [(a, [i])]
duplicates map = map ^@.. ifolded . filtered ((> 1) . length)


missing :: forall a i. (Bounded a, Enum a, Ord a) => Map a [i] -> [a]
missing map = universe `Set.difference` keySet & Set.toList
  where
    keySet :: Set a
    keySet = map & Map.keys & Set.fromAscList

    universe :: Set a
    universe = enumerate
