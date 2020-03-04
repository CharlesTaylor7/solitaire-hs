module Solitaire.App where

import Solitaire.Imports

newtype App e a = App { unApp :: ExceptT e IO a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO)

runApp :: Exception e => App e a -> IO a
runApp = undefined
