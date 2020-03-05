module Solitaire.Sandbox where

import Solitaire.Imports
import Control.Monad.Morph

type f ~> g = forall a. f a -> g a
swap :: (MonadTrans t1, MonadTrans t2) => t1 (t2 Identity) ~> t2 (t1 Identity)
