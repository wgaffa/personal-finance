{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Core.App (
    App (..),
    AppEnvironment (..),
    withDatabase,
) where

import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    MonadThrow,
    bracket,
 )
import Control.Monad.Except (
    ExceptT,
    MonadError,
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (
    MonadReader (ask),
    ReaderT,
 )

import Database.SQLite.Simple (
    Connection,
    close,
    open,
 )

import Core.Database (enableForeignKeys)
import Core.Error (AccountError)
import OptParser (Command)

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

newtype App a = App
    { runApp :: ReaderT AppEnvironment (ExceptT AccountError IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving newtype (MonadReader AppEnvironment, MonadError AccountError)
    deriving newtype (MonadThrow, MonadCatch, MonadMask)

withDatabase :: (Connection -> App a) -> App a
withDatabase f =
    ask >>= \cfg ->
        bracket
            (liftIO . open $ connectionString cfg)
            (liftIO . close)
            (\conn -> liftIO (enableForeignKeys conn) >> f conn)
