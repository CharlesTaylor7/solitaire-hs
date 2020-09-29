module Solitaire.Yukon.Rules
  ( Yukon
  ) where

import Solitaire.Prelude
import Solitaire.Core.Rules
import Solitaire.Core.Utils
  ( pileCountsSize
  , getDeck
  , totalCards
  , cards
  )

import Solitaire.Core.Move.Class (moveType)
import Solitaire.Core.Card (splitAtFirstRun)
import Solitaire.Core.Utils (toPile)

import Solitaire.Yukon.Types hiding (Game, Config)
import Solitaire.Yukon.Moves
import qualified Solitaire.Yukon.Types as Yukon

import qualified Data.Vector as V
import qualified Data.IntMap as M


import Debug.Trace


data Yukon


instance Rules Yukon where

  type Game Yukon = Yukon.Game
  type Config Yukon = Yukon.Config

  moveTypes =
    [ moveType @FlipCard
    , moveType @MoveStack
    , moveType @MoveToFoundation
    ]

  newGame :: (MonadIO m, MonadReader Yukon.Config m) => m Yukon.Game
  newGame = do
    shuffled <- getDeck >>= shuffle
    pileCounts <- view #piles
    let
      (piles, _) = foldl'
        (\(ps, cs) count ->
          let
            size = pileCountsSize count
            (p, cs') = splitAt size cs
          in
            (toPile p count : ps, cs'))
        ([], shuffled)
        pileCounts
      tableau = Tableau $ indexFrom 0 $ reverse piles
      foundation = Foundation mempty
    pure $ Yukon.Game tableau foundation

  gameIsWon :: Yukon.Game -> Bool
  gameIsWon game = game ^. #tableau . to totalCards . to (== 0)

  heuristic :: Yukon.Game -> MoveCount
  heuristic = view
    $ #tableau
    . #_Tableau
    . folded -- piles
    . folded -- vectors
    . to (MoveCount . length)
