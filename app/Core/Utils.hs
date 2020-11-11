{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Utils
    ( maybeToEither
    , today
    ) where

import Data.Data

import Data.Time

import Database.SQLite.Simple (
    SQLData(SQLInteger), ResultError(ConversionFailed)
    )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.FromField
    (returnError,  fieldData, FromField(..) )

import Utility.Absolute ( AbsoluteValue, absoluteValue )

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x

instance (Num a, Typeable a) => FromField (AbsoluteValue a) where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . absoluteValue . fromIntegral $ i
            _ -> returnError ConversionFailed f "need a int"

today :: IO Day
today = utctDay <$> getCurrentTime
