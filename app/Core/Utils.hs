module Core.Utils
    ( maybeToEither
    , prompt
    , promptExcept
    ) where

import System.IO ( stdout, hFlush )

import Control.Monad.Except
    ( MonadIO(liftIO), ExceptT, liftEither )

import Database.SQLite.Simple ( SQLData(SQLInteger) )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.FromField
    ( fieldData, FromField(..) )

import Utility.Absolute ( AbsoluteValue, absoluteValue )

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= liftEither . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

instance (Num a) => FromField (AbsoluteValue a) where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . absoluteValue . fromIntegral $ i
