{-# options_ghc -Wno-deprecations #-}
module PrettyPrinter
  ( prettyPrint
  , tracePretty
  , pretty
  , Pretty(..)
  , PrettyExpr(..)
  , WrappedShow(..)
  ) where

import Prelude

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Debug.Trace (trace)

import Data.Foldable (for_, toList)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Map as Map


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

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . putStrLn . T.unpack . pretty

tracePretty :: Pretty a => a -> b -> b
tracePretty = trace . T.unpack . pretty


-- instances
newtype WrappedShow a = WrappedShow a

instance Show a => Pretty (WrappedShow a) where
  prettyExpr (WrappedShow a) = PrettyStr . T.pack . show $ a

deriving via WrappedShow Bool instance Pretty Bool

instance Pretty Text where
  prettyExpr = PrettyStr

instance Pretty a => Pretty [a] where
  prettyExpr [] = "[]"
  prettyExpr xs =
    PrettyHardWrap
      [ "["
      , PrettyIndent (PrettyHardWrap $ map prettyExpr xs)
      , "]"
      ]

instance Pretty a => Pretty (Vector a) where
  prettyExpr = prettyExpr . toList

instance (Pretty key, Pretty value) => Pretty (Map key value) where
  prettyExpr xs =
    PrettyHardWrap
      [ "{"
      , PrettyIndent $ join xs
      , "}"
      ]
    where
      format (key, value) =
        PrettyHardWrap
          [ PrettySoftWrap [prettyExpr key, ":"]
          , prettyExpr value
          ]
      join = PrettyHardWrap . map format . Map.toList

instance (Pretty a) => Pretty (Maybe a) where
  prettyExpr (Just x) = prettyExpr x
  prettyExpr Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyExpr (Left x) = prettyExpr x
  prettyExpr (Right x) = prettyExpr x

instance Pretty Char where
  prettyExpr = PrettyStr . T.singleton
