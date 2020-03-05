module Solitaire.App where

import Solitaire.Imports

type Log = String
newtype App e a = App { unApp :: ExceptT e (WriterT Log IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadRandom
    , MonadWriter Log
    )

eitherToIO :: Exception e => Either e a -> IO a
eitherToIO (Left e) = throwIO e
eitherToIO (Right a) = pure a

runApp :: Exception e => App e a -> IO a
runApp app = do
  (a, w) <- app
    & unApp
    & runExceptT
    & runWriterT
  putStrLn $ "Log: " ++ w
  eitherToIO a
