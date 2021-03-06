{-# options_ghc -Wno-orphans #-}
{-# options_ghc -Wno-unused-imports #-}
module Solitaire.Core.Types where

import Solitaire.Prelude

import Solitaire.Core.Card (IsCard(..))

import qualified Data.ByteString as BS
import qualified Data.DList as DL

import qualified Rainbow

data Game card foundation stock = Game
  { tableau :: Tableau card
  , foundation :: foundation
  , stock :: stock
  }
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Hashable)

instance (PrettyCard card, Pretty foundation) => Pretty (Game card foundation stock) where
  prettyExpr game =
    PrettyHardWrap
      [ game ^. #foundation . to prettyExpr
      , game ^. #tableau    . to prettyExpr
      ]


-- | Config
data GameConfig = GameConfig
  { numSets :: Int
  , piles :: IntMap (Pile Int)
  }
  deriving (Eq, Show, Generic)


-- | Score
newtype Score = Score Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (Pretty)


-- | Card
data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (Hashable)

instance Enum Card where
  toEnum number =
    let (rank, suit) = number `quotRem` enumSize @Suit
    in Card (rank ^. enum) (suit ^. enum)

  fromEnum card =
    let
      rank = card ^. #rank . from enum
      suit = card ^. #suit . from enum
    in
      rank * (enumSize @Suit) + suit

instance Bounded Card where
  minBound = Card minBound minBound
  maxBound = Card maxBound maxBound

instance IsCard Card where
  isSuccessorOf :: Card -> Card -> Bool
  a `isSuccessorOf` b =
    -- a has a rank 1 higher than b
    a ^. #rank . from enum - b ^. #rank . from enum == 1 &&
    -- a is the opposite color of b
    a ^. #suit . to suitColor /= b ^. #suit . to suitColor

instance Pretty Card where
  prettyExpr card =
    card
    & cardToChunk
    & pure
    & chunksToByteStrings
    & fromList
    & PrettyStr
    where
      cardToChunk :: Card -> Rainbow.Chunk
      cardToChunk card =
        (rank : suit : [])
          & fromString
          & Rainbow.fore textColor

        where
          rank = card ^. #rank . to rankToChar
          suit = card ^. #suit . to suitToChar

          textColor :: Rainbow.Radiant
          textColor =
            case card ^. #suit . to suitColor of
              Red -> Rainbow.red
              Black -> Rainbow.black

instance PrettyCard Card where
  prettyWidth = 2

suitColor :: Suit -> Color
suitColor Hearts = Red
suitColor Diamonds = Red
suitColor _ = Black


data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving stock (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

rankToChar :: Rank -> Char
rankToChar Ten = 'T'
rankToChar Jack = 'J'
rankToChar Queen = 'Q'
rankToChar King = 'K'
rankToChar Ace = 'A'
rankToChar rank = rank & fromEnum & (+1) & intToDigit


data Suit
 = Diamonds
 | Clubs
 | Hearts
 | Spades
  deriving stock (Eq, Ord, Read, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

suitToChar :: Suit -> Char
suitToChar Hearts = 'H'
suitToChar Diamonds = 'D'
suitToChar Spades = 'S'
suitToChar Clubs = 'C'


data Color
 = Red
 | Black
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


-- | Pile
data Pile a = Pile
  { faceUp :: a
  , faceDown :: a
  }
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

newtype IsFaceUp = IsFaceUp Bool
  deriving stock (Eq, Show)
  deriving anyclass (Pretty)

instance FunctorWithIndex IsFaceUp Pile
instance FoldableWithIndex IsFaceUp Pile
instance TraversableWithIndex IsFaceUp Pile where
  itraverse = itraverseOf (traversed & reindexed (IsFaceUp . (== 0)))


-- | Tableau
newtype Tableau card = Tableau (IntMap (Pile (Vector card)))
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (Hashable)

instance PrettyCard card => Pretty (Tableau card) where
  prettyExpr = PrettyHardWrap . map prettyExpr . toRows


-- | Row
newtype Row card = Row [CardView card]
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance PrettyCard card => Pretty (Row card) where
  prettyExpr = PrettyStr . DL.intercalate ["|"] . map pretty . view #_Row


-- | CardView
data CardView card
  = None
  | FaceDown
  | FaceUp card
  deriving (Eq, Show)

instance PrettyCard card => Pretty (CardView card) where
  prettyExpr None = repeatChar @card ' '
  prettyExpr FaceDown = repeatChar @card '-'
  prettyExpr (FaceUp card) = prettyExpr card

repeatChar :: forall card. PrettyCard card => Char -> PrettyExpr
repeatChar = fromString . replicate (prettyWidth @card)


-- | MoveCount
newtype MoveCount = MoveCount Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving (Monoid, Semigroup) via Sum Int
  deriving anyclass (Pretty)

-- pretty helpers
newtype RowCount = RowCount Int

rightPad :: Int -> a -> [a] -> [a]
rightPad n filler list =
  let
    k = n - length list
  in
    list <> replicate k filler

toCardViews :: RowCount -> Pile (Vector card) -> [CardView card]
toCardViews (RowCount n) =
  let
    getFaceUps = toListOf (#faceUp . reversed . folded . to FaceUp)
    getFaceDowns = flip replicate FaceDown . length . view #faceDown
  in
    rightPad n None . (getFaceDowns <> getFaceUps)

toRows :: Tableau card -> [Row card]
toRows (Tableau tableau) =
  let
    rowCount =
      tableau
        & fmap pileSize
        & maximum
        & RowCount
    columns = toCardViews rowCount <$> toList tableau
    rows = transpose columns & map Row

    -- pileSize :: Pile (Vector card) -> Int
    pileSize pile = (pile ^. #faceUp . to length) + (pile ^. #faceDown . to length)
  in
    rows

-- orphan instance for Hashable Intmap & Hashable Vector
-- TODO: Use newtype wrappers?
newtype SomeIndexedFoldable f a i = SomeIndexedFoldable (f a)
  deriving (Generic)

instance (FoldableWithIndex i f, Hashable i, Hashable a) => Hashable (SomeIndexedFoldable f a i) where
  hashWithSalt salt as =
    as ^.. #_SomeIndexedFoldable . ifolded . withIndex
    & hashWithSalt salt

deriving via (SomeIndexedFoldable IntMap a Int) instance Hashable a => Hashable (IntMap a)
deriving via (SomeIndexedFoldable (Map k) v k) instance (Hashable k, Hashable v) => Hashable (Map k v)
deriving via (SomeIndexedFoldable Vector a Int) instance Hashable a => Hashable (Vector a)

