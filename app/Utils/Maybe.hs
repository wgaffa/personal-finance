{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Maybe (
    maybeToEither,
) where

import Data.Data

import Database.SQLite.Simple (
    ResultError (ConversionFailed),
    SQLData (SQLInteger),
 )
import Database.SQLite.Simple.FromField (
    FromField (..),
    fieldData,
    returnError,
 )
import Database.SQLite.Simple.Ok (Ok (Ok))

import Utility.Absolute (AbsoluteValue, absoluteValue)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x

instance (Num a, Typeable a) => FromField (AbsoluteValue a) where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . absoluteValue . fromIntegral $ i
            _ -> returnError ConversionFailed f "need a int"
