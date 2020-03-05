module Solitaire.App where

import Solitaire.Imports

type Log = String
newtype App e a = App { unApp :: WriterT Log (ExceptT e IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadRandom
    , MonadWriter Log
    )

runApp :: Exception e => App e a -> IO a
runApp app =
  app
  & unApp
  & runWriterT
  & runExceptT
  >>= \case
    Left e -> throwIO e
    Right (a, w) -> do
      putStrLn $ "Log: " ++ w
      pure a
