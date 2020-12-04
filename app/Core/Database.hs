{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Database (
    currentAccountingPeriod,
    saveAccount,
    saveTransaction,
    saveJournal,
    findAccount,
    findLedger,
    allAccounts,
    allAccountTransactions,
    allTransactions,
    updateDatabase,
    schemaVersion,
    latestSchemaVersion,
    foreignKeysViolations,
    enableForeignKeys,
    newPeriod,
) where

import Data.Maybe (fromMaybe, isJust)
import Data.Time (Day)

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (
    MonadError (throwError),
    MonadIO (liftIO),
    unless,
 )
import Control.Monad.Trans.Maybe (MaybeT (..))

import Database.SQLite.Simple (
    Connection,
    FromRow (..),
    Only (Only, fromOnly),
    ResultError (ConversionFailed),
    SQLData (SQLInteger, SQLText),
    execute,
    executeMany,
    execute_,
    field,
    lastInsertRowId,
    query,
    query_,
    withTransaction,
 )
import Database.SQLite.Simple.FromField (
    FromField (..),
    fieldData,
    returnError,
 )
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))

import qualified Data.Text as Text

import Core.Error
import Expense.Account
import Expense.Transaction

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
            (SQLInteger i) ->
                Ok
                    . fromMaybe emptyAccountNumber
                    . accountNumber
                    . fromIntegral
                    $ i
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

instance FromRow Details where
    fromRow = Details <$> field <*> field

instance FromField a => FromRow (LedgerEntry a) where
    fromRow = LedgerEntry <$> fromRow <*> fromRow

instance (FromField a) => FromRow (JournalEntry a) where
    fromRow = JournalEntry <$> fromRow <*> fromRow

instance (FromField a) => FromRow (TransactionAmount a) where
    fromRow = TransactionAmount <$> field <*> field

instance ToField AccountName where
    toField = toField . unAccountName

instance ToField AccountElement where
    toField = toField . show

saveAccount ::
    (MonadError AccountError m, MonadIO m) =>
    Account ->
    Connection ->
    m Account
saveAccount acc@Account {..} conn =
    checkAccount number conn
        >> liftIO (insertAccount acc conn)
        >> return acc

{- | Save a transaction to a journal id.
 This uses the last saved accounting period in the database
 to determine the accounting period.
 Consider maybe have the option to pass in the accounting period
-}
saveTransaction ::
    (MonadError AccountError m, MonadIO m) =>
    AccountNumber ->
    -- |Journal Id
    Int ->
    TransactionAmount Int ->
    Connection ->
    m ()
saveTransaction number journalId amount conn = do
    typeId <-
        liftIO $
            runMaybeT $
                transactionTypeId (fst transactionTuple) conn
    (periodId, _) <- liftIO $ currentPeriod conn
    liftIO $
        execute
            conn
            q
            ( number
            , journalId
            , typeId
            , snd transactionTuple
            , periodId
            )
  where
    transactionTuple = (\(TransactionAmount t a) -> (t, a)) amount
    q =
        "insert into transactions\
        \ (account_id, journal_id, type_id, amount, period_id)\
        \ values (?, ?, ?, ?, ?)"

currentPeriod :: (MonadIO m, MonadFail m) => Connection -> m (Int, String)
currentPeriod conn = do
    results <- liftIO (query_ conn q :: IO [(Int, String)])
    case results of
        [] -> fail "no period exists"
        [x] -> return x
        _ -> error "A query returned more than one result"
  where
    q = "SELECT id, name FROM accounting_periods WHERE id=(SELECT MAX(id) FROM accounting_periods)"

-- | Get the total balance for an account and return the closing transaction
closingStatement :: (MonadIO m) => Connection -> Account -> m (JournalEntry Int)
closingStatement conn acc = do
    ledger <- liftIO (allAccountTransactions acc conn :: IO (Ledger Int))
    return . JournalEntry acc . inverse . accountBalance $ ledger
  where
    inverse (TransactionAmount t a) =
        case t of
            Debit -> credit a
            Credit -> debit a

{- | Iterate through all accounts and get their closing statment
 and return an aggregated Journal for all statements
-}
closeAllAccounts :: (MonadIO m) => Connection -> Details -> m (Journal Int)
closeAllAccounts conn details = do
    accs <- liftIO $ allAccounts conn
    entries <- forM accs $ closingStatement conn
    return $ Journal details entries

-- | Retrieve the journal for closing an accounting period and save it
closePeriod :: (MonadIO m, MonadError AccountError m) => Connection -> Details -> m Int
closePeriod conn details = liftIO (closeAllAccounts conn details) >>= flip saveJournal conn

newPeriod ::
    (MonadIO m, MonadError AccountError m) =>
    Connection ->
    -- | Closing details
    Details ->
    -- | Opening details
    Details ->
    -- | Name of new accounting period
    String ->
    m ()
newPeriod conn closeDetails openDetails periodName = do
    accs <- liftIO $ allAccounts conn
    entries <- forM accs $ \x -> do
        ledger <- liftIO (allAccountTransactions x conn :: IO (Ledger Int))
        return . JournalEntry x . accountBalance $ ledger
    _ <- closePeriod conn closeDetails
    _ <- liftIO $ savePeriod conn periodName
    _ <- saveJournal (Journal openDetails entries) conn
    return ()

savePeriod :: (MonadIO m) => Connection -> String -> m Int
savePeriod conn p =
    fromIntegral
        <$> (liftIO (execute conn q (Only p)) >> liftIO (lastInsertRowId conn))
  where
    q = "INSERT INTO accounting_periods (name) values (?)"

saveJournal ::
    (MonadError AccountError m, MonadIO m) =>
    Journal Int ->
    Connection ->
    m Int
saveJournal (Journal details entries) conn = do
    journalId <- insertJournal
    forM_ entries (`insertEntry` journalId)
    return journalId
  where
    insertJournal =
        fromIntegral
            <$> ( liftIO (execute conn qJournal (date details, description details))
                    >> liftIO (lastInsertRowId conn)
                )

    insertEntry (JournalEntry acc amount) journalId = saveTransaction (number acc) journalId amount conn

    qJournal =
        "insert into journals \
        \(date, note) values (?, ?)"

allTransactions :: (FromField a) => Connection -> IO [TransactionAmount a]
allTransactions conn = query_ conn q
  where
    q =
        "select ty.name, t.amount FROM transactions t \
        \INNER JOIN transactiontypes ty ON t.type_id=ty.id \
        \WHERE t.period_id=(SELECT MAX(id) FROM accounting_periods)"

allAccountTransactions ::
    (FromField a) =>
    Account ->
    Connection ->
    IO (Ledger a)
allAccountTransactions acc@Account {..} conn =
    Ledger acc
        <$> query conn q (Only number)
  where
    q =
        "SELECT j.date, j.note, ty.name, t.amount \
        \FROM transactions t \
        \INNER JOIN transactiontypes ty ON t.type_id=ty.id \
        \INNER JOIN journals j ON t.journal_id=j.id \
        \WHERE account_id=? AND t.period_id=(SELECT MAX(id) FROM accounting_periods)\
        \ORDER BY j.date"

checkAccount ::
    (MonadError AccountError m, MonadIO m) =>
    AccountNumber ->
    Connection ->
    m ()
checkAccount number conn =
    liftIO (accountExists number conn)
        >>= \x -> when x (throwError $ AccountNotSaved "account number already exists")

insertAccount :: Account -> Connection -> IO ()
insertAccount Account {..} conn =
    runMaybeT (elementId element conn)
        >>= \x -> execute conn q (number, name, x)
  where
    q = "insert into Accounts (id, name, element_id) values (?, ?, ?)"

accountExists :: AccountNumber -> Connection -> IO Bool
accountExists number conn =
    isJust <$> runMaybeT (findAccount number conn)

allAccounts :: Connection -> IO [Account]
allAccounts conn = query_ conn q
  where
    q =
        "select a.id, a.name, e.name from accounts a \
        \inner join accountelement e on a.element_id=e.id \
        \order by a.id"

findAccount ::
    (MonadFail m, MonadIO m) =>
    AccountNumber ->
    Connection ->
    m Account
findAccount number conn = do
    res <- liftIO (query conn q params :: IO [Account])
    case res of
        (x : _) -> return x
        _ -> fail "no record found"
  where
    q =
        "select a.id, a.name, e.name from accounts a \
        \inner join accountelement e on a.element_id=e.id where a.id=?"
    params = Only number

findLedger ::
    (MonadFail m, MonadIO m, FromField a) =>
    AccountNumber ->
    Connection ->
    m (Ledger a)
findLedger number conn =
    liftIO $
        findAccount number conn
            >>= flip allAccountTransactions conn

{- | Get the current accounting period.
 This selects the last period in the database table
-}
currentAccountingPeriod :: (MonadFail m, MonadIO m) => Connection -> m String
currentAccountingPeriod conn = snd <$> currentPeriod conn

-- | Find the id of an account element in the database
elementId ::
    (MonadFail m, MonadIO m) =>
    AccountElement ->
    Connection ->
    m Int
elementId element conn = do
    r <- liftIO $ query conn q (Only (show element))
    case r of
        (x : _) -> return . fromOnly $ x
        _ -> fail "no record found"
  where
    q = "select id from AccountElement where name=?"

transactionTypeId ::
    (MonadFail m, MonadIO m) =>
    TransactionType ->
    Connection ->
    m Int
transactionTypeId transaction conn = do
    r <- liftIO $ query conn q (Only (show transaction))
    case r of
        (x : _) -> return . fromOnly $ x
        _ -> fail "no record found"
  where
    q = "select id from TransactionTypes where name=?"

updateDatabase :: Connection -> IO ()
updateDatabase conn = do
    ts <- tables conn
    unless ("meta_schema" `elem` ts) $ createMetaTable conn
    v <- schemaVersion conn
    let updates = drop v schema
        funcs = zipWith (flip runVersion) updates [v + 1 ..]
     in mapM_ ($ conn) funcs

runVersion :: Int -> [Connection -> IO ()] -> Connection -> IO ()
runVersion version xs conn =
    withTransaction conn $
        mapM_ (\f -> f conn) xs
            >> updateVersion version conn

createMetaTable :: Connection -> IO ()
createMetaTable conn = execute_ conn q >> execute_ conn val
  where
    q =
        "create table meta_schema (id integer primary key, \
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
    firstItem (x : _) = x

tables :: Connection -> IO [String]
tables conn = map fromOnly <$> query_ conn q
  where
    q =
        "select name from sqlite_master where type='table' \
        \and name not like 'sqlite%'"

latestSchemaVersion :: Int
latestSchemaVersion = length schema

foreignKeysViolations :: Connection -> IO [(String, Int, String, Int)]
foreignKeysViolations conn =
    query_ conn "PRAGMA foreign_key_check"

enableForeignKeys :: Connection -> IO ()
enableForeignKeys conn = execute_ conn "PRAGMA foreign_keys=ON"

schema :: [[Connection -> IO ()]]
schema =
    [
        [ -- version 1
          flip execute_ "PRAGMA foreign_keys = ON"
        , flip
            execute_
            "CREATE TABLE AccountElement (\
            \ id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
        , flip
            execute_
            "CREATE TABLE Accounts (\
            \id INTEGER PRIMARY KEY,\
            \name TEXT NOT NULL,\
            \element_id INTEGER NOT NULL,\
            \FOREIGN KEY(element_id) REFERENCES AccountElement (id))"
        , \conn ->
            executeMany
                conn
                "INSERT INTO AccountElement (name) VALUES (?)"
                (map (Only . show) [Asset .. Expenses])
        ]
    ,
        [ -- version 2
          flip
            execute_
            "CREATE TABLE TransactionTypes (\
            \id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
        , \conn ->
            executeMany
                conn
                "INSERT INTO TransactionTypes (name) VALUES (?)"
                (map (Only . show) [Debit .. Credit])
        , flip
            execute_
            "CREATE TABLE Transactions (\
            \id INTEGER PRIMARY KEY,\
            \date DATE NOT NULL,\
            \account_id INTEGER NOT NULL,\
            \type_id INTEGER NOT NULL,\
            \amount INTEGER NOT NULL,\
            \ FOREIGN KEY (account_id) REFERENCES accounts (id) \
            \ FOREIGN KEY (type_id) REFERENCES transactiontypes (id))"
        ]
    ,
        [ -- version 3
          flip execute_ "ALTER TABLE Transactions ADD description TEXT"
        ]
    ,
        [ -- version 4
          flip
            execute_
            "ALTER TABLE Transactions ADD transaction_id\
            \ TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000000'"
        ]
    ,
        [ -- version 5
          flip execute_ "PRAGMA foreign_keys=OFF"
        , flip
            execute_
            "CREATE TABLE Journals (\
            \id INTEGER PRIMARY KEY, date DATE NOT NULL, note TEXT)"
        , flip
            execute_
            "CREATE TABLE new_transactions (\
            \id INTEGER PRIMARY KEY,\
            \journal_id INTEGER NOT NULL,\
            \account_id INTEGER NOT NULL,\
            \type_id INTEGER NOT NULL,\
            \amount INTEGER NOT NULL,\
            \FOREIGN KEY (account_id) REFERENCES accounts (id) \
            \FOREIGN KEY (journal_id) REFERENCES journals (id) \
            \FOREIGN KEY (type_id) REFERENCES transactiontypes (id))"
        , copyTransactionsV4ToV5
        , flip execute_ "DROP TABLE transactions"
        , flip execute_ "ALTER TABLE new_transactions RENAME TO transactions"
        , flip execute_ "PRAGMA foreign_keys=ON"
        ]
    ,
        [ -- version 6
          flip execute_ "PRAGMA foreign_keys=OFF"
        , flip
            execute_
            "CREATE TABLE accounting_periods (\
            \id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
        , flip
            execute_
            "INSERT INTO accounting_periods \
            \VALUES (1, \"Starting period\")"
        , flip
            execute_
            "CREATE TABLE new_transactions (\
            \id INTEGER PRIMARY KEY,\
            \journal_id INTEGER NOT NULL,\
            \account_id INTEGER NOT NULL,\
            \type_id INTEGER NOT NULL,\
            \period_id INTEGER NOT NULL,\
            \amount INTEGER NOT NULL,\
            \FOREIGN KEY (account_id) REFERENCES accounts (id) \
            \FOREIGN KEY (journal_id) REFERENCES journals (id) \
            \FOREIGN KEY (period_id) REFERENCES accounting_periods (id) \
            \FOREIGN KEY (type_id) REFERENCES transactiontypes (id))"
        , copyTransactionsV5ToV6
        , flip execute_ "DROP TABLE transactions"
        , flip execute_ "ALTER TABLE new_transactions RENAME TO transactions"
        , flip execute_ "PRAGMA foreign_keys=ON"
        ]
    ]

copyTransactionsV5ToV6 :: Connection -> IO ()
copyTransactionsV5ToV6 conn =
    execute_
        conn
        "INSERT INTO new_transactions \
        \SELECT id, journal_id, account_id, type_id, 1, amount \
        \FROM transactions"

copyTransactionsV4ToV5 :: Connection -> IO ()
copyTransactionsV4ToV5 conn = do
    transactions <-
        query_
            conn
            "SELECT date, description, transaction_id from transactions \
            \GROUP BY transaction_id \
            \ORDER BY id" ::
            IO [(Day, String, String)]
    forM_ transactions $ \(date, note, uuid) -> do
        execute conn "INSERT INTO journals (date, note) VALUES (?, ?)" (date, note)
        insertId <- lastInsertRowId conn
        execute
            conn
            "INSERT INTO new_transactions SELECT id, ?, account_id, type_id, amount \
            \FROM transactions WHERE transaction_id=?"
            (insertId, uuid)
