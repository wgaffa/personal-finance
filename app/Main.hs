{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO ()

import Data.Maybe (fromMaybe)
import Data.Char ( isSpace )
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Catch ( bracket, finally )
import Control.Monad.Except
    ( MonadTrans(lift),
      MonadIO(liftIO),
      ExceptT,
      liftEither,
      runExceptT )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Control.Monad.Reader
    ( MonadTrans(lift),
      MonadIO(liftIO),
      ReaderT(runReaderT),
      MonadReader(ask) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Database.SQLite.Simple ( close, open, Connection )
import Database.SQLite.Simple.FromField ( FromField )

import OptParser
    ( Command(..),
      ShowOptions(..),
      Options(Options, optCommand, dbConnection),
      execArgParser )
import Core.Database
    ( saveAccount,
      saveTransaction,
      allAccountTransactions,
      allAccounts,
      findAccount,
      updateDatabase )
import Core.PrettyPrint
    ( printAccount, printListAccounts, printLedger )

import Expense.Transaction ( TransactionAmount )
import Expense.Account
    ( AccountTransaction(AccountTransaction),
      Accountable(increase, decrease),
      Account(Account),
      AccountNumber,
      Ledger(..),
      accountName,
      accountNumber )

import Core.Utils ( maybeToEither, promptExcept )
import Core.Error
    ( AccountError(InvalidNumber, AccountNotFound, InvalidName,
                   InvalidElement, MiscError, ParseError) )
import Utility.Absolute ( unAbsoluteValue )

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

type App = ReaderT AppEnvironment (ExceptT AccountError IO)

main :: IO ()
main = do
    env <- readEnvironment
    runner <- runExceptT . flip runReaderT env . dispatcher . command $ env
    either perror return runner
  where
    perror x = putStrLn $ "Error: " ++ show x

dispatcher :: Command -> App ()
dispatcher List = showAccounts
dispatcher CreateAccount = createAccount
dispatcher AddTransaction = addTransaction
dispatcher (ShowAccount n) = showTransactions n
dispatcher UpdateDatabase = updateDb

readEnvironment :: IO AppEnvironment
readEnvironment = do
    Options {..} <- execArgParser
    return AppEnvironment {
        connectionString = fromMaybe "db.sqlite3" dbConnection
        , command = optCommand
        }

updateDb :: App ()
updateDb = do
    cfg <- ask
    liftIO $ bracket
        (open $ connectionString cfg)
        close
        updateDatabase
    liftIO $ putStrLn "Database updated"

showAccounts :: App ()
showAccounts = do
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    accounts <- liftIO $ allAccounts conn
    liftIO $ printListAccounts accounts

showTransactions :: ShowOptions -> App ()
showTransactions ShowOptions{..} =
    (fmap unAbsoluteValue
        <$> ((liftEither
                . maybeToEither InvalidNumber
                . accountNumber $ filterAccount)
            >>= findLedger))
    >>= liftIO . printLedger

createAccount :: App ()
createAccount = do
    acc <- lift createAccountInteractive
    liftIO $ putStrLn "Attempting to save account"
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    res <- lift $ finally (saveAccount acc conn) (liftIO $ close conn)
    liftIO $ putStr "Saved account: "
            >> printAccount res
            >> putChar '\n'

addTransaction :: App ()
addTransaction = do
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    account <- lift $ findAccountInteractive conn
    transaction <- lift $ createTransactionInteractive account
    lift $ finally
        (saveTransaction account transaction conn)
        (liftIO $ close conn)

findLedger :: (FromField a) => AccountNumber -> App (Ledger a)
findLedger number = do
    cfg <- ask
    ledger <- liftIO $ bracket
        (open $ connectionString cfg)
        close
        (\conn -> do
            account <- runMaybeT $ findAccount number conn
            case account of
                Just x ->
                    Right . Ledger x
                    <$> allAccountTransactions x conn
                Nothing -> pure . Left $ AccountNotFound
        )
    liftEither ledger

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: "
        (maybeToEither InvalidName . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

findAccountInteractive :: Connection -> ExceptT AccountError IO Account
findAccountInteractive conn =
    promptExcept "Account number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    >>= liftIO . runMaybeT . flip findAccount conn
    >>= liftEither . maybeToEither (MiscError "account not found")

createTransactionInteractive ::
    (Accountable a) =>
    a
    -> ExceptT AccountError IO (AccountTransaction Int)
createTransactionInteractive x =
    AccountTransaction
    <$> promptExcept "Date: "
        (maybeToEither ParseError . readMaybe)
    <*> promptExcept "Description: "
        (pure . emptyString)
    <*> (fmap (truncate . (*100)) <$> createTransactionAmountInteractive x)
  where
    emptyString xs
        | all isSpace xs = Nothing
        | otherwise = Just xs

createTransactionAmountInteractive ::
    (Accountable a)
    => a
    -> ExceptT AccountError IO (TransactionAmount Double)
createTransactionAmountInteractive x =
    createAccountTransactionAmount x
    <$> promptExcept "Amount: " (maybeToEither InvalidNumber . readMaybe)

createAccountTransactionAmount ::
    (Accountable a, Ord b, Num b)
    => a -> b -> TransactionAmount b
createAccountTransactionAmount x m
    | m < 0 = decrease x m
    | otherwise = increase x m