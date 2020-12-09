{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module Main where

import System.IO ()

import Data.Char (isSpace)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad (forM)
import Control.Monad.Except (
    MonadError (),
    MonadIO (liftIO),
    liftEither,
    runExceptT,
    throwError,
 )
import Control.Monad.Reader (
    ReaderT (runReaderT),
 )
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))

import Database.SQLite.Simple (
    Connection,
    withTransaction,
 )

import Command.CheckHealth
import Core.Database
import Core.PrettyPrint
import OptParser

import Expense.Account
import Expense.Transaction

import Core.App
import Core.Config (Build, Config, buildConfig)
import Core.Error
import Core.Prompt
import Utils

main :: IO ()
main = do
    (cfg, cmd) <- readCmdLine
    res <-
        runExceptT
            . flip runReaderT (buildConfig cfg)
            . runApp
            . dispatcher
            $ cmd
    either perror return res
  where
    perror x = putStrLn $ "Error: " ++ show x

dispatcher :: Command -> App ()
dispatcher List = showAccounts
dispatcher CreateAccount = createAccount
dispatcher AddTransaction = addTransaction
dispatcher (ShowAccount n) = showTransactions n
dispatcher UpdateDatabase = updateDb
dispatcher CheckHealth = runTasks checkHealth
dispatcher AccountingPeriod = showAccountingPeriod
dispatcher (NewAccountingPeriod s) = newAccountingPeriod s

readCmdLine :: IO (Config Build, Command)
readCmdLine = do
    options <- execArgParser
    return (config options, optCommand options)

newAccountingPeriod :: String -> App ()
newAccountingPeriod str
    | all isSpace str = throwError $ MiscError "no printable characters in string"
newAccountingPeriod str = do
    now <- liftIO today
    withDatabase $
        \conn ->
            newPeriod
                conn
                (Details now (Just "Closing Statement"))
                (Details now (Just "Opening Statement"))
                (Text.unpack . Text.strip . Text.pack $ str)

updateDb :: App ()
updateDb = do
    withDatabase $ liftIO . updateDatabase
    liftIO $ putStrLn "Database updated"

showAccountingPeriod :: App ()
showAccountingPeriod = do
    period <- withDatabase currentAccountingPeriod
    liftIO $ putStrLn period

showAccounts :: App ()
showAccounts = do
    withDatabase $
        liftIO . \conn -> do
            accounts <- allAccounts conn
            ledgers <-
                forM
                    accounts
                    (\Account {..} -> findLedger number conn) ::
                    IO [Ledger Int]
            let triage = map (\l@(Ledger a _) -> (a, value . accountBalance $ l)) ledgers
             in putStr $ renderTriageBalance triage
  where
    value (TransactionAmount _ a) = a

showTransactions :: ShowOptions -> App ()
showTransactions ShowOptions {..} =
    ( ( liftEither
            . maybeToEither InvalidNumber
            . accountNumber
            $ filterAccount
      )
        >>= ( \number ->
                withDatabase (liftIO . findLedger number)
            ) ::
        App (Ledger Int)
    )
        >>= liftIO . putStrLn . renderLedger

createAccount :: App ()
createAccount = do
    acc <- createAccountInteractive
    liftIO $ putStrLn "Attempting to save account"
    res <- withDatabase (saveAccount acc)
    liftIO $
        putStr "Saved account: "
            >> printAccount res
            >> putChar '\n'

addTransaction :: App ()
addTransaction = do
    now <- liftIO today
    date <- promptDate "Date: " now
    desc <- promptExcept "Note: " $ pure . emptyString
    journal <- transactionInteractive $ Journal (Details date desc) []
    _ <-
        withDatabase $
            liftIO . \conn -> do
                withTransaction conn $ do
                    res <- runExceptT $ saveJournal journal conn
                    case res of
                        (Right i) -> pure i
                        (Left _) -> error "Unexpected error when writing journal"
    return ()

createAccountInteractive ::
    (MonadError AccountError m, MonadIO m) =>
    m Account
createAccountInteractive =
    Account
        <$> promptExcept
            "Number: "
            (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
        <*> promptExcept
            "Name: "
            (maybeToEither InvalidName . accountName . Text.pack)
        <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

findAccountInteractive ::
    (MonadError AccountError m, MonadIO m) =>
    Connection ->
    m Account
findAccountInteractive conn =
    promptExcept
        "Account number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
        >>= liftIO . runMaybeT . flip findAccount conn
        >>= liftEither . maybeToEither AccountNotFound

createTransactionAmountInteractive ::
    (Accountable a, MonadError AccountError m, MonadIO m) =>
    a ->
    m (TransactionAmount Double)
createTransactionAmountInteractive x =
    createAccountTransactionAmount x
        <$> promptExcept "Amount: " (maybeToEither InvalidNumber . readMaybe)

-- | Take an 'Accountable' a and increase or decrease it based on b
createAccountTransactionAmount ::
    (Accountable a, Num b, Eq b) =>
    a ->
    b ->
    TransactionAmount b
createAccountTransactionAmount x m
    | signum m == -1 = decrease x . abs $ m
    | otherwise = increase x . abs $ m

transactionInteractive ::
    Journal Int ->
    App (Journal Int)
transactionInteractive journal@(Journal details _) =
    findAccountInDatabase >>= readAccount >>= readEntries
  where
    findAccountInDatabase =
        withDatabase $ \conn -> do
            acc <- findAccountInteractive conn
            liftIO $ printAccount acc
            return acc
    readAccount account =
        Journal details . (: entries journal) . JournalEntry account
            <$> ( fmap (round . (* 100))
                    <$> createTransactionAmountInteractive account
                )
    readEntries x =
        (liftIO . putStrLn . renderJournal $ x)
            >> (pure . balance . map amount $ entries x)
            >>= ( \currentBalance ->
                    if currentBalance == 0
                        then pure x
                        else transactionInteractive x
                )
    entries (Journal _ xs) = xs
    amount (JournalEntry _ x) = x

emptyString :: String -> Maybe String
emptyString xs
    | all isSpace xs = Nothing
    | otherwise = Just xs
