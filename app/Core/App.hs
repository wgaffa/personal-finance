{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Core.App
    ( App(..)
    , AppEnvironment(..)
    , withDatabase
    ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader
    (ReaderT, MonadReader(ask))
import Control.Monad.Except
    (ExceptT, MonadError)
import Control.Monad.Catch
    (MonadThrow, MonadCatch, MonadMask, bracket)

import Database.SQLite.Simple
    ( close,
      open,
      Connection
    )

import Core.Database (enableForeignKeys)
import OptParser (Command)
import Core.Error (AccountError)

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

newtype App a = App {
    runApp :: ReaderT AppEnvironment (ExceptT AccountError IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving newtype (MonadReader AppEnvironment, MonadError AccountError)
    deriving newtype (MonadThrow, MonadCatch, MonadMask)

withDatabase :: (Connection -> App a) -> App a
withDatabase f = ask >>= \ cfg -> bracket
    (liftIO . open $ connectionString cfg)
    (liftIO . close)
    (\ conn -> liftIO (enableForeignKeys conn) >> f conn)
