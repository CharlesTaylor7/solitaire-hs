{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
module Solitaire.Core.Move where

import Solitaire.Prelude hiding (Map)

import qualified Data.Sum as FastSum


class IsMove game move where
  type InvalidMove move :: *
  apply :: MonadError (InvalidMove move) m => move -> game -> m game


type family Map (f :: * -> *) (xs :: [*]) where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs


class IsMove1 (game :: *) (constMoveFunctor :: * -> *) where
  apply1 :: MonadError (InvalidMove move) m => constMoveFunctor Void -> game -> m game

instance IsMove1 game (Const move) => IsMove game move where
  type InvalidMove move = InvalidMove move
  apply move game = apply1 @game @(Const move) (Const @move @Void move) game



-- instance FastSum.Apply (IsMove1 game) moves => IsMove1 game (FastSum.Sum moves) where
  -- apply1 move game = FastSum.apply @(IsMove1 game) (apply1 move game)
