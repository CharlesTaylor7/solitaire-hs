{-# options_ghc -Wno-deprecations #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-unused-imports #-}
module PrettyPrinter
  ( prettyPrint
  , pretty
  -- , tracePretty
  , chunksToByteStrings
  , Pretty(..)
  , SomePretty(..)
  , PrettyCard(..)
  , PrettyExpr(..)
  , KeyValuePairs(..)
  -- re export
  , DList
  ) where

import Prelude

import Control.Lens (ifor_)

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

-- | types
class Pretty a where
  prettyExpr :: a -> PrettyExpr
  default prettyExpr :: Show a => a -> PrettyExpr
  prettyExpr = fromString . show


class Pretty card => PrettyCard card where
  -- | width of the pretty printed card text
  prettyWidth :: Int

data PrettyExpr
  = PrettyStr (DList ByteString)
  | PrettySoftWrap [PrettyExpr]
  | PrettyHardWrap [PrettyExpr]
  | PrettyIndent PrettyExpr

instance IsString PrettyExpr where
  fromString = PrettyStr . DL.singleton . fromString


prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint a = liftIO $ do
  traverse_ BS.putStr $ pretty a
  putStrLn ""

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
  ifor_ exprs $ \i expr -> do
    -- print newlines for all but the first line
    when (i /= 0) $ tell (DL.singleton "\n")
    -- print the indentation
    tell $ DL.singleton indentation
    -- print the expression
    toPrettyM expr


-- instances
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


-- | A wrapper type for map like objects to be pretty printed
newtype KeyValuePairs key value = KeyValuePairs [(key, value)]

instance (Pretty key, Pretty value) => Pretty (KeyValuePairs key value) where
  prettyExpr (KeyValuePairs pairs) =
    PrettyHardWrap
      [ "{"
      , PrettyIndent $ join pairs
      , "}"
      ]
    where
      format (key, value) =
        PrettyHardWrap
          [ PrettySoftWrap [prettyExpr key, ":"]
          , prettyExpr value
          ]
      join = PrettyHardWrap . map format

instance (Pretty key, Pretty value) => Pretty (Map key value) where
  prettyExpr = prettyExpr . KeyValuePairs . Map.toList

instance (Pretty a) => Pretty (Maybe a) where
  prettyExpr (Just x) = prettyExpr x
  prettyExpr Nothing = "Nothing"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyExpr (Left x) = prettyExpr x
  prettyExpr (Right x) = prettyExpr x

instance (Pretty a, Pretty b) => Pretty (a, b) where
  prettyExpr (a, b) =
    PrettyHardWrap
      [ prettyExpr a
      , prettyExpr b
      ]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  prettyExpr (a, b, c) =
    PrettyHardWrap
      [ prettyExpr a
      , prettyExpr b
      , prettyExpr c
      ]

instance Pretty Int
instance Pretty Float
instance Pretty Char where
  prettyExpr = prettyExpr . T.singleton

data SomePretty =
  forall p. Pretty p => SomePretty p

instance Pretty SomePretty where
  prettyExpr (SomePretty p) = prettyExpr p
