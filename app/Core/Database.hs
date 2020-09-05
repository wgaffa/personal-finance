{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Database
    ( saveAccount
    , saveTransaction
    , findAccount
    , allAccounts
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import qualified Data.Text as Text

import Core.Error
import Expense.Transaction
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
            (SQLInteger i) -> Ok
                . maybe emptyAccountNumber id
                . accountNumber . fromIntegral $ i
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

saveAccount :: Account -> Connection -> ExceptT AccountError IO Account
saveAccount acc@Account{..} conn =
    checkAccount (unAccountNumber number) conn
    >> lift (insertAccount acc conn)
    >> ExceptT (return $ Right acc)

saveTransaction ::
    Account
    -> AccountTransaction Int
    -> Connection
    -> ExceptT AccountError IO ()
saveTransaction Account{..} AccountTransaction{..} conn = do
    typeId <- liftIO $ runMaybeT $
        transactionTypeId (fst $ transactionTuple) conn
    liftIO $ execute conn q (number, typeId, date, snd transactionTuple)
  where
    transactionTuple = (\(TransactionAmount t a) -> (t, a)) $ amount
    q = "insert into transactions\
        \ (account_id, type_id, date, amount) values (?, ?, ?)"

checkAccount :: Int -> Connection -> ExceptT AccountError IO ()
checkAccount number conn =
    liftIO (accountExists number conn)
    >>= \x -> when x (throwError $ AccountNotSaved "account number already exists")

insertAccount :: Account -> Connection -> IO ()
insertAccount Account{..} conn =
    runMaybeT (elementId element conn)
    >>= \x -> (execute conn q (number, name, x))
  where
    q = "insert into Accounts (id, name, element_id) values (?, ?, ?)"

accountExists :: Int -> Connection -> IO Bool
accountExists number conn =
    runMaybeT (findAccount number conn)
    >>= return . maybe False (const True)

allAccounts :: Connection -> IO [Account]
allAccounts conn = query_ conn q
  where q = "select a.id, a.name, e.name from accounts a \
    \inner join accountelement e on a.element_id=e.id \
    \order by a.id"

findAccount :: Int -> Connection -> MaybeT IO Account
findAccount number conn = do
    res <- liftIO $ (query conn q params :: IO [Account])
    case res of
        (x:_) -> return x
        _ -> MaybeT . return $ Nothing
  where
    q = "select a.id, a.name, e.name from accounts a \
        \inner join accountelement e on a.element_id=e.id where a.id=?"
    params = Only number

-- | Find the id of an account element in the database
elementId :: AccountElement -> Connection -> MaybeT IO Int
elementId element conn = do
    r <- liftIO $ query conn q (Only (show element))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> MaybeT . return $ Nothing
  where q = "select id from AccountElement where name=?"

transactionTypeId :: TransactionType -> Connection -> MaybeT IO Int
transactionTypeId transaction conn = do
    r <- liftIO $ query conn q (Only (show transaction))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> MaybeT . return $ Nothing
  where
    q = "select id from TransactionTypes where name=?"