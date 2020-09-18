{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Core.Database
    ( saveAccount
    , saveTransaction
    , findAccount
    , allAccounts
    , allAccountTransactions
    , updateDatabase
    ) where

import Data.Maybe (fromMaybe, isJust)

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.Except
    ( when,
      unless,
      MonadTrans(lift),
      MonadIO(liftIO),
      ExceptT(..),
      MonadError(throwError) )
import Control.Monad.IO.Class (liftIO)

import Database.SQLite.Simple
    ( execute,
      executeMany,
      execute_,
      query,
      query_,
      withTransaction,
      field,
      Only(Only, fromOnly),
      SQLData(SQLText, SQLInteger),
      ResultError(ConversionFailed),
      FromRow(..),
      Connection )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.ToField ( ToField(..) )
import Database.SQLite.Simple.FromField
    ( ResultError(ConversionFailed),
      fieldData,
      returnError,
      FromField(..) )

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
                . fromMaybe emptyAccountNumber
                . accountNumber . fromIntegral $ i
            _ -> returnError ConversionFailed f "need an int"

instance FromField AccountElement where
    fromField f =
        case fieldData f of
            (SQLText s) -> Ok . read . Text.unpack $ s
            _ -> returnError ConversionFailed f "need a string"

instance FromField TransactionType where
    fromField f =
        case fieldData f of
            (SQLText s) -> Ok . read . Text.unpack $ s
            _ -> returnError ConversionFailed f "need a string"

instance FromRow Account where
    fromRow = Account <$> field <*> field <*> field

instance (FromField a) => FromRow (AccountTransaction a) where
    fromRow = AccountTransaction <$> field <*> field <*> fromRow

instance (FromField a) => FromRow (TransactionAmount a) where
    fromRow = TransactionAmount <$> field <*> field

instance ToField AccountName where
    toField = toField . unAccountName

instance ToField AccountElement where
    toField = toField . show

saveAccount ::
    (MonadError AccountError m, MonadIO m)
    => Account -> Connection -> m Account
saveAccount acc@Account{..} conn =
    checkAccount number conn
    >> liftIO (insertAccount acc conn)
    >> return acc

saveTransaction ::
    (MonadError AccountError m, MonadIO m)
    => Account
    -> AccountTransaction Int
    -> Connection
    -> m ()
saveTransaction Account{..} AccountTransaction{..} conn = do
    typeId <- liftIO $ runMaybeT $
        transactionTypeId (fst transactionTuple) conn
    liftIO $ execute conn q
        (number,
        maybe Text.empty Text.pack description,
        typeId, date, snd transactionTuple)
  where
    transactionTuple = (\(TransactionAmount t a) -> (t, a)) amount
    q = "insert into transactions\
        \ (account_id, description, type_id, date, amount)\
        \ values (?, ?, ?, ?, ?)"

allAccountTransactions ::
    (FromField a) =>
    Account
    -> Connection
    -> IO [AccountTransaction a]
allAccountTransactions Account{..} conn =
    query conn q (Only number)
  where
    q = "select t.date, t.description, ty.name, t.amount from transactions t \
        \inner join transactiontypes ty on t.type_id=ty.id \
        \where account_id=? order by t.date"

checkAccount ::
    (MonadError AccountError m, MonadIO m)
    => AccountNumber -> Connection -> m ()
checkAccount number conn =
    liftIO (accountExists number conn)
    >>= \x -> when x (throwError $ AccountNotSaved "account number already exists")

insertAccount :: Account -> Connection -> IO ()
insertAccount Account{..} conn =
    runMaybeT (elementId element conn)
    >>= \x -> execute conn q (number, name, x)
  where
    q = "insert into Accounts (id, name, element_id) values (?, ?, ?)"

accountExists :: AccountNumber -> Connection -> IO Bool
accountExists number conn =
    isJust <$> runMaybeT (findAccount number conn)

allAccounts :: Connection -> IO [Account]
allAccounts conn = query_ conn q
  where q = "select a.id, a.name, e.name from accounts a \
    \inner join accountelement e on a.element_id=e.id \
    \order by a.id"

findAccount ::
    (MonadFail m, MonadIO m)
    => AccountNumber -> Connection -> m Account
findAccount number conn = do
    res <- liftIO (query conn q params :: IO [Account])
    case res of
        (x:_) -> return x
        _ -> fail "no record found"
  where
    q = "select a.id, a.name, e.name from accounts a \
        \inner join accountelement e on a.element_id=e.id where a.id=?"
    params = Only number

-- | Find the id of an account element in the database
elementId ::
    (MonadFail m, MonadIO m)
    => AccountElement -> Connection -> m Int
elementId element conn = do
    r <- liftIO $ query conn q (Only (show element))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> fail "no record found"
  where q = "select id from AccountElement where name=?"

transactionTypeId ::
    (MonadFail m, MonadIO m)
    => TransactionType -> Connection -> m Int
transactionTypeId transaction conn = do
    r <- liftIO $ query conn q (Only (show transaction))
    case r of
        (x:_) -> return . fromOnly $ x
        _ -> fail "no record found"
  where
    q = "select id from TransactionTypes where name=?"

updateDatabase :: Connection -> IO ()
updateDatabase conn = do
    ts <- tables conn
    unless ("meta_schema" `elem` ts) $ createMetaTable conn
    v <- schemaVersion conn
    let updates = drop v schema
        funcs = zipWith (flip runVersion) updates [v+1 ..]
        in mapM_ ($ conn) funcs

runVersion :: Int -> [Connection -> IO ()] -> Connection -> IO ()
runVersion version xs conn = withTransaction conn $
    mapM_ (\f -> f conn) xs
    >> updateVersion version conn

createMetaTable :: Connection -> IO ()
createMetaTable conn = execute_ conn q >> execute_ conn val
  where
    q = "create table meta_schema (id integer primary key, \
        \version integer not null)"
    val = "insert into meta_schema values (1, 0)"

updateVersion :: Int -> Connection -> IO ()
updateVersion version conn = execute conn q (Only version)
  where
    q = "update meta_schema set version=? where id=1"

schemaVersion :: Connection -> IO Int
schemaVersion conn = fromOnly . firstItem <$> query_ conn q
  where
    q = "select version from meta_schema where id=1"
    firstItem [] = error "Database is in an invalid state"
    firstItem (x:_) = x

tables :: Connection -> IO [String]
tables conn = map fromOnly <$> query_ conn q
  where
    q = "select name from sqlite_master where type='table' \
        \and name not like 'sqlite%'"

schema :: [[Connection -> IO ()]]
schema =
    [[
      flip execute_ "PRAGMA foreign_keys = ON"
    , flip execute_ "CREATE TABLE AccountElement (\
        \ id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    , flip execute_ "CREATE TABLE Accounts (\
        \id INTEGER PRIMARY KEY,\
        \name TEXT NOT NULL,\
        \element_id INTEGER NOT NULL,\
        \FOREIGN KEY(element_id) REFERENCES AccountElement (id))"
    , \conn -> executeMany conn
        "INSERT INTO AccountElement (name) VALUES (?)"
        (map (Only . show) [Asset .. Expenses])
    ]
    , [
      flip execute_ "CREATE TABLE TransactionTypes (\
        \id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    , \conn -> executeMany conn
        "INSERT INTO TransactionTypes (name) VALUES (?)"
        (map (Only . show) [Debit .. Credit])
    , flip execute_ "CREATE TABLE Transactions (\
        \id INTEGER PRIMARY KEY,\
        \date DATE NOT NULL,\
        \account_id INTEGER NOT NULL,\
        \type_id INTEGER NOT NULL,\
        \amount INTEGER NOT NULL,\
        \ FOREIGN KEY (account_id) REFERENCES accounts (id) \
        \ FOREIGN KEY (type_id) REFERENCES transactiontypes (id))"
    ]
    , [
        flip execute_ "ALTER TABLE Transactions ADD description TEXT"
    ]]
