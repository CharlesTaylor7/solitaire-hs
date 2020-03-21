{-# LANGUAGE StandaloneDeriving #-}
module Solitaire.Internal.OrdOrphans where

import Prelude (Ord)
import Solitaire.Internal.Types

deriving instance Ord Game
deriving instance Ord Foundation
deriving instance Ord Layout
deriving instance Ord a => Ord (Pile a)
