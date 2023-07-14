{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Core.App (
    App (..),
    AppEnvironment (..),
) where

import System.Console.Haskeline (InputT)

import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    MonadThrow,
 )
import Control.Monad.Except (
    ExceptT,
    MonadError,
 )
import Control.Monad.IO.Class (MonadIO ())
import Control.Monad.Reader (
    MonadReader (),
    ReaderT,
 )

import Core.Error (AccountError)
import OptParser (Command)

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

newtype App a = App
    { runApp :: ReaderT AppEnvironment (ExceptT AccountError (InputT IO)) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving newtype (MonadReader AppEnvironment, MonadError AccountError)
    deriving newtype (MonadThrow, MonadCatch, MonadMask, MonadFail)
