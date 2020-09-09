{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO

import Data.Char
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Database.SQLite.Simple

import Data.Time

import Repl
import OptParser
import Core.Database
import Core.PrettyPrint

import Expense.Transaction
import Expense.Account

import Core.Utils
import Core.Error
import Utility.Absolute

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

type App = ReaderT AppEnvironment (ExceptT AccountError IO)

main :: IO ()
main = do
    env <- readEnvironment
    runner <- runExceptT . flip runReaderT env . dispatcher . command $ env
    either perror (return . id) runner
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
    (Options {..}) <- execArgParser
    return AppEnvironment {
        connectionString = maybe "db.sqlite3" id dbConnection
        , command = optCommand
        }

updateDb :: App ()
updateDb = do
    cfg <- ask
    liftIO $ bracket
        (open $ connectionString cfg)
        (close)
        (updateDatabase)
    liftIO $ putStrLn "Database updated"

showAccounts :: App ()
showAccounts = do
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    accounts <- liftIO $ allAccounts conn
    liftIO $ printListAccounts accounts

showTransactions :: ShowOptions -> App ()
showTransactions ShowOptions{..} = do
    cfg <- ask
    ledger <- liftIO $ bracket
        (open $ connectionString cfg)
        (close)
        (\conn -> do
            account <- runMaybeT $ findAccount filterAccount conn
            case account of
                Just x ->
                    (allAccountTransactions x conn
                        :: IO [AccountTransaction (AbsoluteValue Int)])
                    >>= pure . Just . Ledger x
                Nothing -> pure Nothing
        )
    case ledger of
        Just x -> liftIO $ printLedger $ transformLedger x
        Nothing -> return ()
  where
    transformLedger (Ledger account ts) =
        Ledger account
        $ map (\x@AccountTransaction{..} ->
             x{amount = fmap unAbsoluteValue amount}) ts

createAccount :: App ()
createAccount = do
    acc <- lift $ createAccountInteractive
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
    transaction <- lift $ createTransactionInteractive
    lift $ finally
        (saveTransaction account transaction conn)
        (liftIO $ close conn)

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: "
        (maybeToEither InvalidNumber . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

findAccountInteractive :: Connection -> ExceptT AccountError IO Account
findAccountInteractive conn =
    promptExcept "Account number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    >>= liftIO . runMaybeT . flip findAccount conn . unAccountNumber
    >>= liftEither . maybeToEither (MiscError "account not found")

createTransactionInteractive ::
    ExceptT AccountError IO (AccountTransaction Int)
createTransactionInteractive =
    AccountTransaction
    <$> promptExcept "Date: "
        (maybeToEither ParseError . readMaybe)
    <*> promptExcept "Description: "
        (pure . emptyString)
    <*> (createTransactionAmountInteractive
        >>= return . fmap (truncate . (*100)))
  where
    emptyString xs
        | all isSpace xs = Nothing
        | otherwise = Just xs

createTransactionAmountInteractive ::
    ExceptT AccountError IO (TransactionAmount Double)
createTransactionAmountInteractive =
    TransactionAmount
    <$> promptExcept "Debit/Credit: "
        (maybeToEither InvalidTransactionType . readMaybe)
    <*> promptExcept "Amount: "
        (maybeToEither InvalidNumber . readMaybe)