{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Database (
    withDatabase,
) where

import Data.Data (Typeable)

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))

import Database.SQLite.Simple (
    Connection,
    ResultError (ConversionFailed),
    SQLData (SQLInteger),
    close,
    open,
 )
import Database.SQLite.Simple.FromField (
    FromField (..),
    fieldData,
    returnError,
 )
import Database.SQLite.Simple.Ok (Ok (Ok))

import Core.App
import Core.Config (Run (fromRun), database)
import Core.Database (enableForeignKeys)
import Utility.Absolute (AbsoluteValue, absoluteValue)

instance (Num a, Typeable a) => FromField (AbsoluteValue a) where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . absoluteValue . fromIntegral $ i
            _ -> returnError ConversionFailed f "need a int"

withDatabase :: (Connection -> App a) -> App a
withDatabase f =
    ask >>= \cfg ->
        bracket
            (liftIO . open $ fromRun . database $ cfg)
            (liftIO . close)
            (\conn -> liftIO (enableForeignKeys conn) >> f conn)
