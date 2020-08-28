{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Database
    ( elementId
    , saveAccount
    , findAccount
    , withConnectionGeneral
    ) where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import qualified Data.Text as Text

import Expense.Account

instance ToField AccountNumber where
    toField = toField . unAccountNumber

instance FromField AccountName where
    fromField f =
        case fieldData f of
            (SQLText s) -> case accountName s of
                (Just x) -> Ok x
                Nothing -> returnError ConversionFailed f "string is null"
            _ -> returnError ConversionFailed f "need a string"

instance FromField AccountNumber where
    fromField f =
        case fieldData f of
            (SQLInteger i) -> Ok . maybe emptyAccountNumber id . accountNumber . fromIntegral $ i
            _ -> returnError ConversionFailed f "need an int"

instance FromField AccountElement where
    fromField f =
        case fieldData f of
            (SQLText s) -> Ok . read . Text.unpack $ s
            _ -> returnError ConversionFailed f "need a string"

instance FromRow Account where
    fromRow = Account <$> field <*> field <*> field

instance ToField AccountName where
    toField = toField . unAccountName

instance ToField AccountElement where
    toField = toField . (+1) . fromEnum

instance ToRow Account where
    toRow (Account number name element) = toRow (number, name, element)

saveAccount :: Account -> Connection -> IO ()
saveAccount Account{..} conn =
    runMaybeT (elementId element conn)
    >>= \x -> return (number, name, x)
    >>= execute conn q
  where
    q = "insert into Accounts (id, name, element_id) values (?, ?, ?)"

findAccount :: Int -> Connection -> MaybeT IO Account
findAccount number conn = do
    res <- liftIO $ (query conn q params :: IO [Account])
    case res of
        (x:_) -> return x
        _ -> MaybeT . return $ Nothing
  where
    q = "select a.id, a.name, e.name from accounts a inner join accountelement e on a.element_id=e.id where a.id=?"
    params = Only number

-- | Find the id of an account element in the database
elementId :: AccountElement -> Connection -> MaybeT IO Int
elementId element conn = do
    r <- liftIO $ query conn q (Only (show element))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> MaybeT . return $ Nothing
  where q = "select id from AccountElement where name=?"

-- | Run with connection in a MaybeT context
-- | MaybeT is not part of MonadUnliftIO (not following the laws?) so we
-- | can use this function instead as an easier to read function call
withConnectionGeneral :: String -> (Connection -> MaybeT IO a) -> MaybeT IO a
withConnectionGeneral db exec = 
    MaybeT $ do
        withConnection db (\c -> runMaybeT (exec c))
