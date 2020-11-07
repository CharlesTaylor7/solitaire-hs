module LinearRegression
  (
  ) where

import Prelude
import GHC.Generics
import Data.Text (Text)
import Data.Data
import Data.Data.Lens
import Control.Lens
import Data.Generics.Labels ()

import Data.Set (Set)
import qualified Data.Set as Set

data Point a = Point
  { x :: a
  , y :: a
  } deriving stock (Generic)

data Line a = Line
  { m :: a
  , b :: a
  } deriving stock (Generic)



simpleLinearRegression :: [Point a] -> Line a
simpleLinearRegression = undefined



data Term where
  Zero :: Term
  One :: Term
  Lit :: Float -> Term
  Var :: Text -> Term
  Add :: Term -> Term -> Term
  Sub :: Term -> Term -> Term
  Times :: Term -> Term -> Term
    deriving stock (Data, Generic)

instance Plated Term

partialD :: Text -> Term -> Term
partialD _ Zero = Zero
partialD _ One = Zero
partialD _ (Lit _) = Zero
partialD text (Var name)
  | text == name = One
  | otherwise = Zero

-- sum rule
partialD t (Add x y) = Add (partialD t x) (partialD t y)
partialD t (Sub x y) = Sub (partialD t x) (partialD t y)

-- product rule
partialD t (Times x y) =
  Add
    (partialD t x `Times` y)
    (x `Times` partialD t y)


rules :: Term -> Maybe Term

-- literal evaluation
rules (Add (Lit x) (Lit y)) = Just $ Lit $ x + y
rules (Sub (Lit x) (Lit y)) = Just $ Lit $ x - y
rules (Times (Lit x) (Lit y)) = Just $ Lit $ x * y

-- identity laws
rules (Add Zero lit) = Just $ lit
rules (Add lit Zero) = Just $ lit
rules (Sub Zero lit) = Just $ lit
rules (Sub lit Zero) = Just $ lit
rules (Times One lit) = Just $ lit
rules (Times lit One) = Just $ lit

-- annihilator
rules (Times Zero lit) = Just $ Zero
rules (Times lit Zero) = Just $ Zero

-- catch all
rules _ = Nothing

simplify :: Term -> Term
simplify = rewrite rules

toSetOf :: Getting (Set a) s a -> s -> Set a
toSetOf l s = getConst (l (\x -> Const (Set.singleton x)) s)

eval :: Term -> Either (Set Text) Float
eval term =
  let
    simplified = simplify term
  in
    case simplify term of
      Lit val -> Right val
      term' -> Left $ term' & toSetOf (cosmos . #_Var)
