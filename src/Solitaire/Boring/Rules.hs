{-# language FlexibleInstances #-}
{-# options_ghc -Wno-unused-top-binds #-}
module Solitaire.Boring.Rules
  ( Boring
  , scorePile
  ) where

import Solitaire.Prelude
import Solitaire.Core.Rules
import Solitaire.Core.Move
import Solitaire.Core.Utils (toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (IsCard(..), Run(..), splitIntoRuns)

import Solitaire.Boring.Types
import Solitaire.Boring.Moves
import qualified Solitaire.Boring.Types as Boring


data Boring


instance Rules Boring where
  type Game Boring = Boring.Game
  type Config Boring = Boring.Config

  moveTypes =
    [ moveType @FlipCard
    , moveType @MoveStack
    , moveType @MoveToFoundation
    ]


  newGame :: (MonadIO m, MonadReader Boring.Config m) => m Boring.Game
  newGame = do
    shuffled <- getDeck >>= shuffle
    pileCounts <- view #piles
    let
      piles = fst $ foldl'
        (\(ps, cs) count ->
          let
            size = pileCountsSize count
            (p, cs') = splitAt size cs
          in
            (toPile p count : ps, cs'))
        ([], shuffled)
        pileCounts
      tableau = Tableau $ indexFrom 0 $ reverse piles
      foundation = Foundation 0
    pure $ Boring.Game tableau foundation

  gameIsWon :: Boring.Game -> Bool
  gameIsWon game = game ^. #tableau . to totalCards . to (== 0)



-- scoring game states, for A* path finding
-- TODO: account for foundation
scoreGame :: Boring.Game -> Score
scoreGame = undefined


scoreRun :: Run card -> Score
scoreRun (Run cards) = Score $ length cards - 1

scorePile :: (IsCard card, Foldable f) => Pile (f card) -> Score
scorePile pile =
  pile & sumOf (#faceUp . to toList . to splitIntoRuns . traverse . to scoreRun)

scoreByRuns :: Boring.Game -> Score
scoreByRuns game =
  game & sumOf (#tableau . #_Tableau . traverse . to scorePile)
