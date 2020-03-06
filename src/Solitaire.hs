{--
Bare bones simple solitaire solver.
Prototype is a simplified Spider solitaire.
Cards are suitless and have rank between 1 to 5
There are 3 piles of 5 cards each with the top 2 rows face up.
--}
module Solitaire
  ( module Solitaire
  , module Solitaire.Exports
  ) where

import Solitaire.Exports
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

runGame :: Env -> IO ()
runGame env =
  flip runReaderT env $
    newGame >>= loopM act

printS :: MonadIO m => String -> m ()
printS = liftIO . putStrLn

userConfirm :: (MonadIO m) => m ()
userConfirm = liftIO getLine $> ()

userInput :: (MonadIO m, Read a) => m a
userInput = read <$> liftIO getLine

act :: (MonadIO m, MonadReader Env m, MonadRandom m) => Game -> m (Either Game ())
act game = do
  prettyPrint game
  userConfirm
  steps <- validSteps game
  printS "Valid moves:"
  prettyPrint $ map _step_move steps
  next <- randomElem steps
  case next of
    Nothing -> do
      printS "No valid moves"
      pure $ Right ()
    Just (Step move game) -> do
      printS "Chose move: "
      prettyPrint move
      pure $ Left game

newGame :: (MonadIO m, MonadRandom m, MonadReader Env m) => m Game
newGame = do
  deck <- getDeck
  shuffled <- shuffleIO deck
  pileSizes <- getPileSizes
  let
    piles = fst $ foldl'
      (\(ps, cs) size ->
        let (p, cs') = splitAt size cs
        in (toPile p : ps, cs'))
      ([], deck)
      pileSizes
    layout = Layout $ indexFrom 0 piles
    foundation = Foundation 0
  pure $ Game layout foundation

getPileSizes :: MonadReader Env m => m [Int]
getPileSizes = do
  p <- view env_numPiles
  s <- view env_numSets
  let
    n = enumSize @Card
    (q, r) = (s * n) `divMod` p
    piles = replicate (p - r) q ++ replicate r (q + 1)
  pure piles

getDeck :: (MonadReader Env m) => m [Card]
getDeck = do
  numSets <- view env_numSets
  pure $ enumerate >>= replicate numSets

toPile :: [Card] -> Pile
toPile cards =
  let (faceUp, faceDown) = splitAt 2 cards
  in Pile (V.fromList faceUp) (V.fromList faceDown)

indexFrom :: Int -> [a] -> IntMap a
indexFrom offset = M.fromAscList . zip [offset..]

shuffleIO :: (MonadIO m, MonadRandom m) => [Card] -> m [Card]
shuffleIO coll = do
  let vector = V.fromList coll
  thawed <- liftIO $ V.thaw vector
  shuffleIOVector thawed
  frozen <- liftIO $ V.freeze thawed
  pure $ V.toList (frozen :: Vector Card)

shuffleIOVector :: (MonadIO m, MonadRandom m) => IOVector a -> m ()
shuffleIOVector vector =
  let
    n = MV.length vector
    indices = [0..(n-1)] :: [Int]
  in
    for_ indices $ \i ->
      getRandomR (0, i) >>=
      liftIO . MV.swap vector i
