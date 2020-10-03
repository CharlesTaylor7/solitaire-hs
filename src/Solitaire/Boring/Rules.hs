{-# options_ghc -Wno-unused-top-binds #-}
module Solitaire.Boring.Rules
  ( Boring
  ) where

import Solitaire.Prelude
import Solitaire.Core.Rules
import Solitaire.Core.Move.Class
import Solitaire.Core.Utils (toPile, totalCards, pileCountsSize, getDeck, cardsRemaining)
import Solitaire.Core.Card (IsCard(..), Run(..), splitIntoRuns)

import Solitaire.Boring.Types
import Solitaire.Boring.Moves
import qualified Solitaire.Boring.Types as Boring


data Boring


instance Rules Boring where
  type Config Boring = Boring.Config
  type Foundation Boring = Boring.Foundation
  type Card Boring = Boring.Card


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
      foundation = Foundation 0
    pure $ Boring.Game tableau foundation


  gameIsWon :: Boring.Game -> Bool
  gameIsWon game = game ^. #tableau . to totalCards . to (== 0)


  heuristicFeatures :: Boring.Game -> Map Text Float
  heuristicFeatures _ = mempty
