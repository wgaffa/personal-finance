{-# LANGUAGE OverloadedStrings #-}

module Core.Database
    ( elementId
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import Expense.Account

instance ToField AccountNumber where
    toField = toField . unAccountNumber

instance FromField AccountNumber where
    fromField f = case fieldData f of
        (SQLInteger i) -> Ok . maybe emptyAccountNumber id . accountNumber . fromIntegral $ i
        _ -> returnError ConversionFailed f "need an int"

instance ToField AccountName where
    toField = toField . unAccountName

instance ToField AccountElement where
    toField = toField . fromEnum

instance ToRow Account where
    toRow (Account number name element) = toRow (number, name, element)

saveAccount :: Account -> IO ()
saveAccount = undefined

-- | Find the id of an account element in the database
elementId :: AccountElement -> Connection -> IO Int
elementId element conn = do
    r <- query conn q (Only (show element))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> error "Database is invalid, check the integrity of the AccountElement table"
  where q = "select id from AccountElement where name=?"