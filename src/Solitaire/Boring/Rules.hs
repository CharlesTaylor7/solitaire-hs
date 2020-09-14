{-# language FlexibleInstances #-}
{-# options_ghc -Wno-unused-top-binds #-}
module Solitaire.Boring.Rules
  ( Boring
  , scorePile
  ) where

import Solitaire.Prelude
import Solitaire.Core.Rules
import Solitaire.Core.Utils (cards, toPile, totalCards, pileCountsSize, getDeck)
import Solitaire.Core.Card (IsCard(..), Run(..), splitIntoRuns, splitAtFirstRun)

import Solitaire.Boring.PrettyInstances ()
import Solitaire.Boring.Types
import qualified Solitaire.Boring.Types as Boring

import qualified Data.Vector as V
import qualified Data.IntMap as M

import Debug.Trace


data Boring

-- boilerplate instances
-- these are required in each solitaire implementation
-- because type applications are not allowed in the instances head
-- otherwise I would just derive this once at App's declaration
deriving newtype instance MonadReader Boring.Config (App Boring)
deriving newtype instance MonadHistory Boring.Game (App Boring)


instance Rules Boring where
  type Game Boring = Boring.Game
  type Config Boring = Boring.Config
  type Move Boring = Boring.Move
  type InvalidMove Boring = Boring.InvalidMove

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

  moves :: NumPiles -> [Boring.Move]
  moves (NumPiles numPiles) =
    let
      range = [0..numPiles-1]
      moves = moveStack <$> range <*> range
      flips = flipCard <$> range
      sets = moveToFoundation <$> range
    in
      sets <> flips <> moves


  moveReducer
    :: (MonadError Boring.InvalidMove m)
    => Boring.Move
    -> Boring.Game
    -> m Boring.Game
  moveReducer move =
    case move of
      FlipCard (FC i) ->
        #tableau . #_Tableau . ix i $ \pile -> do
          if is _Just $ pile ^? #faceUp . _Cons
          then throwError (CardFlipOnUnexposedPile i)
          else pure ()

          (head, rest) <- pile ^? #faceDown . _Cons
            & (maybeToError $ CardFlipOnEmptyPile i)

          let faceUp' = #faceUp .~ [head]
          let faceDown' = #faceDown .~ rest
          pure $ pile & faceUp' . faceDown'
      MoveToFoundation (MTF i) ->
        #foundation . #numSets +~ 1 >>>
        (#tableau . #_Tableau . ix i $ \pile ->
          let
            (set, leftover) = pile ^. #faceUp . to splitAtFirstRun
            pile' = pile & #faceUp .~ leftover
          in do
            when (length set /= enumSize @Boring.Card) $ throwError $ IncompleteSet i
            pure pile'
        )
      MoveStack (MS i j) ->
        #tableau . #_Tableau $ \tableau -> do
          let
            (stack, sourcePileRest) =
              tableau ^?! ix i . #faceUp . to splitAtFirstRun
            target = tableau ^?! ix j

          -- sanity checking
          when (i == j) $
            throwError $ SourceIsTarget i

          when (null stack) $
            throwError $ EmptyStackSource i

          -- short circuit case where you move a stack onto an empty pile
          if V.null . cards $ target
          then error "todo"
          else do
            -- Can never move a stack onto facedown cards
            when (is _Empty $ target ^. #faceUp) $
              throwError $ MoveStackOntoFaceDownCards  j

          let targetCard = tableau ^?! ix j . #faceUp . _head
          let stackBottomCard = stack ^?! _last
          let stackSize = length stack
          let diff = fromEnum targetCard - fromEnum stackBottomCard
          let splitStackAt = length stack - diff - 1

          when (splitStackAt < 0 || splitStackAt >= stackSize) $
            throwError $ MismatchingStacks i j
              & traceShow (splitStackAt, stack)

          let
            (stackPartToMove, stackPartToLeave) = V.splitAt splitStackAt stack

            sourceUpdate = ix i . #faceUp .~ (stackPartToLeave <> sourcePileRest)
            targetUpdate = ix j . #faceUp %~ (stackPartToMove <>)

          pure $ tableau & sourceUpdate . targetUpdate

  -- scoring game states, for A* path finding
scoreStep :: Step Boring -> (Score, Score)
scoreStep step = (step ^. #move . to scoreMove, step ^. #game . to scoreByRuns)
  where
    scoreMove :: Boring.Move -> Score
    scoreMove (MoveToFoundation _) = 2
    scoreMove (FlipCard _) = 1
    scoreMove (MoveStack _) = 0

scoreRun :: Run card -> Score
scoreRun (Run cards) = Score $ length cards - 1

scorePile :: (IsCard card, Foldable f) => Pile (f card) -> Score
scorePile pile =
  pile & sumOf (#faceUp . to toList . to splitIntoRuns . traverse . to scoreRun)

scoreByRuns :: Boring.Game -> Score
scoreByRuns game =
  game & sumOf (#tableau . #_Tableau . traverse . to scorePile)
