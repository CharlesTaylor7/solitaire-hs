module Solitaire.App where

import Solitaire.Imports

newtype App e a = App { unApp :: ExceptT e IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadRandom
    )

runApp :: Exception e => App e a -> IO a
runApp app =
  app
  & unApp
  & runExceptT
  >>= \case
    Left e -> throwIO e
    Right a -> pure a
