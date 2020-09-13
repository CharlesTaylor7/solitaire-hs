{-# options_ghc -Wno-orphans #-}
{-# options_ghc -Wno-unused-imports #-}
module Solitaire.Core.Types where

import Solitaire.Prelude

import Solitaire.Core.Card (IsCard(..))

import qualified Data.ByteString as BS
import qualified Data.DList as DL

import qualified Rainbow


-- | Config
data Config = Config
  { numSets :: Int
  , piles :: IntMap (Pile Int)
  }
  deriving (Eq, Show, Generic)


-- | Score
newtype Score = Score Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

deriving via WrappedShow Score instance Pretty Score


-- | Card
data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving stock (Eq, Show, Generic)
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

instance Pretty Card where
  prettyExpr card =
    PrettyStr $ fromList $ chunksToByteStrings [rank, suit]
    where
      rank = card ^. #rank . to rankToChunk
      suit = card ^. #suit . to suitToChunk

      suitToChunk :: Suit -> Rainbow.Chunk
      suitToChunk Hearts = "H"
      suitToChunk Diamonds = "D"
      suitToChunk Spades = "S"
      suitToChunk Clubs = "C"

      rankToChunk :: Rank -> Rainbow.Chunk
      rankToChunk Ten = "T"
      rankToChunk Jack = "J"
      rankToChunk Queen = "Q"
      rankToChunk King = "K"
      rankToChunk Ace = "A"
      rankToChunk rank = rank & fromEnum & (+2) & intToDigit & pure & fromString


instance IsCard Card where
  isSuccessorOf :: Card -> Card -> Bool
  a `isSuccessorOf` b =
    -- a has a rank 1 higher than b
    a ^. #rank . from enum - b ^. #rank . from enum == 1 &&
    -- a is the opposite color of b
    a ^. #suit . to color /= b ^. #suit . to color
    where
      color :: Suit -> Color
      color Hearts = Red
      color Diamonds = Red
      color _ = Black


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
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


data Suit
 = Diamonds
 | Clubs
 | Hearts
 | Spades
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


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
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Hashable)


-- | Tableau
newtype Tableau card = Tableau (IntMap (Pile (Vector card)))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Pretty card => Pretty (Tableau card) where
  prettyExpr = PrettyHardWrap . map prettyExpr . toRows


-- | Row
newtype Row card = Row [CardView card]
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Pretty card => Pretty (Row card) where
  prettyExpr = PrettyStr . DL.intercalate ["|"] . map pretty . view #_Row


-- | CardView
data CardView card
  = None
  | FaceDown
  | FaceUp card
  deriving (Eq, Show)

instance Pretty card => Pretty (CardView card) where
  prettyExpr None = "  "
  prettyExpr FaceDown = "--"
  prettyExpr (FaceUp card) = prettyExpr card


newtype RowCount = RowCount Int


-- pretty helpers
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


-- pretty instances
