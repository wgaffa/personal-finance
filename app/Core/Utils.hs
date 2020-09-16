module Core.Utils
    ( maybeToEither
    , today
    ) where

import Data.Time

import Database.SQLite.Simple ( SQLData(SQLInteger) )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.FromField
    ( fieldData, FromField(..) )

import Utility.Absolute ( AbsoluteValue, absoluteValue )

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x

instance (Num a) => FromField (AbsoluteValue a) where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . absoluteValue . fromIntegral $ i

today :: IO Day
today = getCurrentTime >>= pure . utctDay