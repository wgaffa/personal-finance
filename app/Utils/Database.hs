module Utils.Database (
    withDatabase,
) where

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))

import Database.SQLite.Simple (Connection, close, open)

import Core.App
import Core.Database (enableForeignKeys)

withDatabase :: (Connection -> App a) -> App a
withDatabase f =
    ask >>= \cfg ->
        bracket
            (liftIO . open $ connectionString cfg)
            (liftIO . close)
            (\conn -> liftIO (enableForeignKeys conn) >> f conn)
