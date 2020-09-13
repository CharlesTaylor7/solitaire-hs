{-# language FlexibleInstances #-}
{-# options_ghc -Wno-unused-top-binds #-}
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

import Solitaire.Core.Card (splitAtFirstRun)
import Solitaire.Core.Utils (toPile)

import Solitaire.Yukon.PrettyInstances ()
import Solitaire.Yukon.Types
import qualified Solitaire.Yukon.Types as Yukon

import qualified Data.Vector as V
import qualified Data.IntMap as M


import Debug.Trace


data Yukon

-- boilerplate instances
-- these are required in each solitaire implementation
-- because type applications are not allowed in the instances head
-- otherwise I would just derive this once at App's declaration
deriving newtype instance MonadReader Yukon.Config (App Yukon)
deriving newtype instance MonadHistory Yukon.Game (App Yukon)


instance Rules Yukon where
  type Game Yukon = Yukon.Game
  type Config Yukon = Yukon.Config
  type Move Yukon = Yukon.Move
  type InvalidMove Yukon = Yukon.InvalidMove

  newGame :: (MonadIO m, MonadReader Yukon.Config m) => m Yukon.Game
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
      foundation = Foundation mempty
    pure $ Yukon.Game tableau foundation

  gameIsWon :: Yukon.Game -> Bool
  gameIsWon game = game ^. #tableau . to totalCards . to (== 0)

  moves :: (MonadReader Yukon.Config m) => m [Yukon.Move]
  moves = do
    numPiles <- M.size <$> view #piles
    let
      range = [0..numPiles-1]
      moves = moveStack <$> range <*> range
      flips = flipCard <$> range
      sets = moveToFoundation <$> range
    pure $ sets ++ flips ++ moves

  moveReducer
    :: (MonadError Yukon.InvalidMove m)
    => Yukon.Move
    -> Yukon.Game
    -> m Yukon.Game
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
      MoveToFoundation (MTF _) ->
        error "not implemented"
        -- #foundation . #numSets +~ 1 >>>
        -- (#tableau . #_Tableau . ix i $ \pile ->
        --   let
        --     (set, leftover) = pile ^. #faceUp . to splitAtFirstRun
        --     pile' = pile & #faceUp .~ leftover
        --   in do
        --     when (length set /= enumSize @Yukon.Card) $ throwError $ IncompleteSet i
        --     pure pile'
        -- )
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

