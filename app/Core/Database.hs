{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Database
    ( elementId
    , saveAccount
    , withConnectionGeneral
    ) where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

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

saveAccount :: Account -> Connection -> IO ()
saveAccount Account{..} conn = execute conn q params
  where
    foreignId = elementId $ element
    params = (number, name, element)
    q = "insert into Accounts (id, name, element_id) values (?, ?, ?)"

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
