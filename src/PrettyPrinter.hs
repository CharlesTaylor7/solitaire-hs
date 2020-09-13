{-# options_ghc -Wno-deprecations #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-unused-imports #-}
module PrettyPrinter
  ( prettyPrint
  -- , tracePretty
  , chunksToByteStrings
  , Pretty(..)
  , PrettyExpr(..)
  , WrappedShow(..)
  ) where

import Prelude

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Debug.Trace (trace)

import Data.Foldable (traverse_, for_, toList)
import Data.Function ((&))
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.DList (DList)

import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Map as Map

import qualified Rainbow

import System.IO.Unsafe (unsafePerformIO)


-- tracePretty :: Pretty a => a -> b -> b
-- tracePretty = trace . T.unpack . pretty

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint =
  liftIO .
  traverse_ BS.putStr .
  pretty

pretty :: Pretty a => a -> DList ByteString
pretty = evalPrettyExpr . prettyExpr

chunksToByteStrings :: [Rainbow.Chunk] -> [ByteString]
chunksToByteStrings = Rainbow.chunksToByteStrings terminalPrinter

-- byteStringMakerFromEnvironment :: IO (Chunk -> [ByteString] -> [ByteString])
terminalPrinter :: Rainbow.Chunk -> [ByteString] -> [ByteString]
terminalPrinter = unsafePerformIO Rainbow.byteStringMakerFromEnvironment
{-# NOINLINE terminalPrinter #-}


evalPrettyExpr :: PrettyExpr -> DList ByteString
evalPrettyExpr = snd . runWriter . flip runReaderT 2 . flip evalStateT 0 . toPrettyM

type PrettyM m = (MonadReader Int m, MonadState Int m, MonadWriter (DList ByteString) m)

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
  indentation <- T.encodeUtf8 . flip T.replicate " " <$> get
  for_ (exprs `zip` [0 :: Int ..]) $ \(expr, i) -> do
    -- print newlines for all but the first line
    when (i /= 0) $ tell (DL.singleton "\n")
    -- print the indentation
    tell $ DL.singleton indentation
    -- print the expression
    toPrettyM expr

data PrettyExpr
  = PrettyStr (DList ByteString)
  | PrettySoftWrap [PrettyExpr]
  | PrettyHardWrap [PrettyExpr]
  | PrettyIndent PrettyExpr

instance IsString PrettyExpr where
  fromString = PrettyStr . DL.singleton . fromString

class Pretty a where
  prettyExpr :: a -> PrettyExpr


-- instances
newtype WrappedShow a = WrappedShow a

instance Show a => Pretty (WrappedShow a) where
  prettyExpr (WrappedShow a) = fromString . show $ a

deriving via WrappedShow Bool instance Pretty Bool

instance Pretty Text where
  prettyExpr = PrettyStr . DL.singleton . T.encodeUtf8

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
  prettyExpr = prettyExpr . T.singleton
