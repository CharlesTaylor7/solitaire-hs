module Solitaire.RuleSet where

import Solitaire.Prelude


data Step move game = Step
  { move :: move
  , game :: game
  }
  deriving (Eq, Read, Show, Generic)



class RuleSet ruleset monad where
  data Game ruleset :: *
  data Move ruleset :: *

  newGame :: monad (Game ruleset)
  gameIsWon :: Game ruleset -> Bool


