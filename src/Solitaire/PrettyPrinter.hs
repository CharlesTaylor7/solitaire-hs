{-# OPTIONS_GHC -Wno-deprecations #-}
module Solitaire.PrettyPrinter
  ( prettyPrint
  , tracePretty
  , pretty
  , Pretty(..)
  ) where

import Solitaire.Prelude hiding (Empty)

import Debug.Trace (trace)

import Data.String (IsString(..))

import qualified Data.Text as T

pretty :: Pretty a => a -> Text
pretty = evalPrettyExpr . prettyExpr

evalPrettyExpr :: PrettyExpr -> Text
evalPrettyExpr = snd . runWriter . flip runReaderT 2 . flip evalStateT 0 . toPrettyM

type PrettyM m = (MonadReader Int m, MonadState Int m, MonadWriter Text m)

toPrettyM :: PrettyM m => PrettyExpr -> m ()
toPrettyM (PrettyStr text) = do
  tell text

toPrettyM (PrettyIndent expr) = do
  indent <- ask
  modify (+ indent)
  toPrettyM expr
  modify (subtract indent)

toPrettyM (PrettySoftWrap exprs) =
  for_ exprs toPrettyM

toPrettyM (PrettyHardWrap exprs) = do
  indentation <- flip T.replicate " " <$> get
  for_ (exprs `zip` [0 :: Int ..]) $ \(expr, i) -> do
    when (i /= 0) $ tell "\n"
    tell indentation
    toPrettyM expr

data PrettyExpr
  = PrettyStr Text
  | PrettySoftWrap [PrettyExpr]
  | PrettyHardWrap [PrettyExpr]
  | PrettyIndent PrettyExpr

instance IsString PrettyExpr where
  fromString = PrettyStr . T.pack

class Pretty a where
  prettyExpr :: a -> PrettyExpr

-- instances
newtype WrappedShow a = WrappedShow a

instance Show a => Pretty (WrappedShow a) where
  prettyExpr (WrappedShow a) = PrettyStr . T.pack . show $ a


prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . putStrLn . T.unpack . pretty

tracePretty :: Pretty a => a -> b -> b
tracePretty = trace . T.unpack . pretty
