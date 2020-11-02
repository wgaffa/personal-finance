{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO ()

import Data.Maybe (fromMaybe)
import Data.Char ( isSpace )
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Either (isLeft)

import Control.Monad (when, unless, forM_, forM)
import Control.Monad.Catch
    ( MonadThrow, MonadCatch, MonadMask
    , bracket)
import Control.Monad.Except
    ( MonadIO(liftIO),
      MonadError(),
      ExceptT,
      liftEither,
      runExceptT )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Control.Monad.Reader
    ( ReaderT(runReaderT),
      MonadReader(ask) )

import Database.SQLite.Simple
    ( withTransaction,
      close,
      open,
      Connection
    )

import OptParser
import Core.Database
import Core.PrettyPrint

import Expense.Transaction
import Expense.Account

import Core.Utils
import Core.Prompt
import Core.Error
import Utility.Absolute

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    }

newtype App a = App {
    runApp :: ReaderT AppEnvironment (ExceptT AccountError IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving newtype (MonadReader AppEnvironment, MonadError AccountError)
    deriving newtype (MonadThrow, MonadCatch, MonadMask)

main :: IO ()
main = readEnvironment
    >>= \ env -> (runExceptT
        . flip runReaderT env
        . runApp
        . dispatcher
        . command $ env)
    >>= either perror return
  where
    perror x = putStrLn $ "Error: " ++ show x

dispatcher :: Command -> App ()
dispatcher List = showAccounts
dispatcher CreateAccount = createAccount
dispatcher AddTransaction = addTransaction
dispatcher (ShowAccount n) = showTransactions n
dispatcher UpdateDatabase = updateDb
dispatcher CheckHealth = checkHealth

readEnvironment :: IO AppEnvironment
readEnvironment = do
    Options {..} <- execArgParser
    return AppEnvironment
        { connectionString = fromMaybe "db.sqlite3" dbConnection
        , command = optCommand
        }

checkHealth :: App ()
checkHealth =
    liftIO (putStr "Checking balance: ")
    >> (withDatabase (liftIO . allTransactions) :: App [TransactionAmount Int])
    >>= liftIO . checkBalance
    >> liftIO (putStr "Checking DB version: ")
    >> withDatabase (liftIO . schemaVersion)
    >>= liftIO . checkDatabaseVersion
    >> liftIO (putStr "Checking foreign keys: ")
    >> withDatabase (liftIO . foreignKeysViolations)
    >>= liftIO . checkForeignKeys
  where
    checkBalance xs
      | zeroBalance xs = putStrLn "OK"
      | otherwise = putStrLn "NOT OK"
    checkDatabaseVersion v
      | v == latestSchemaVersion = putStrLn "OK"
      | v < latestSchemaVersion =
          putStrLn $ show v ++ " needs to updated to " ++ show latestSchemaVersion
      | v > latestSchemaVersion =
          putStrLn $ show v ++ " is newer than the latest " ++ show latestSchemaVersion ++
            ", undefined behaviour"
    checkForeignKeys violations
      | null violations = putStrLn "OK"
      | otherwise = putStrLn $ "NOT OK, found " ++ show (length violations) ++ " violations"

updateDb :: App ()
updateDb = do
    withDatabase $ liftIO . updateDatabase
    liftIO $ putStrLn "Database updated"

showAccounts :: App ()
showAccounts = do
    withDatabase $ liftIO . \ conn -> do
        accounts <- allAccounts conn
        ledgers <- forM accounts
          (\ Account{..} -> findLedger number conn) :: IO [Ledger Int]
        let triage = map (\ (Ledger a ts) -> (a, accountBalance a ts)) ledgers
            in putStr $ renderTriageBalance triage
  where
    accountBalance x = value . toBalance x id . balance . map amount
    value (TransactionAmount _ a) = a
    amount (LedgerEntry _ x)= x

showTransactions :: ShowOptions -> App ()
showTransactions ShowOptions{..} =
    (fmap unAbsoluteValue
        <$> ((liftEither
                . maybeToEither InvalidNumber
                . accountNumber $ filterAccount)
            >>= (\ accountNumber ->
                withDatabase (liftIO . findLedger accountNumber)
            )))
    >>= liftIO . putStrLn . renderLedger

createAccount :: App ()
createAccount = do
    acc <- createAccountInteractive
    liftIO $ putStrLn "Attempting to save account"
    res <- withDatabase (saveAccount acc)
    liftIO $ putStr "Saved account: "
            >> printAccount res
            >> putChar '\n'

addTransaction :: App ()
addTransaction = do
    now <- liftIO today
    date <- promptDate "Date: " now
    desc <- promptExcept "Note: " $ pure . emptyString
    journal <- transactionInteractive $ Journal (Details date desc) []
    journalId <- withDatabase $ liftIO . \ conn -> do
        withTransaction conn $ do
            res <- runExceptT $ saveJournal (fmap unAbsoluteValue journal) conn
            case res of
              (Right i) -> pure i
              (Left _) -> error "Unexpected error when writing journal"
    withDatabase $ liftIO . \ conn -> do
        withTransaction conn $ do
            forM_ (entries journal) $ \ acc -> do
                res <- runExceptT $ saveTransaction
                    (number . account $ acc)
                    journalId
                    (unAbsoluteValue <$> amount acc)
                     conn
                when (isLeft res) $
                    error "Unexpected error when saving transaction"
    return ()
  where
    entries (Journal _ xs) = xs
    account (JournalEntry x _) = x
    amount (JournalEntry _ x) = x

createAccountInteractive ::
    (MonadError AccountError m, MonadIO m)
    => m Account
createAccountInteractive = Account
    <$> promptExcept "Number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: "
        (maybeToEither InvalidName . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

findAccountInteractive ::
    (MonadError AccountError m, MonadIO m)
    => Connection
    -> m Account
findAccountInteractive conn =
    promptExcept "Account number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    >>= liftIO . runMaybeT . flip findAccount conn
    >>= liftEither . maybeToEither AccountNotFound

createTransactionAmountInteractive ::
    (Accountable a, MonadError AccountError m, MonadIO m)
    => a
    -> m (TransactionAmount (AbsoluteValue Double))
createTransactionAmountInteractive x =
    createAccountTransactionAmount x
    <$> promptExcept "Amount: " (maybeToEither InvalidNumber . readMaybe)

createAccountTransactionAmount ::
    (Accountable a, Num b, Eq b)
    => a -> b -> TransactionAmount (AbsoluteValue b)
createAccountTransactionAmount x m
    | signum m == -1 = decrease x . absoluteValue $ m
    | otherwise = increase x . absoluteValue $ m

transactionInteractive ::
    Journal (AbsoluteValue Int)
    -> App (Journal (AbsoluteValue Int))
transactionInteractive journal@(Journal details _) =
    findAccountInDatabase >>= readAccount >>= readEntries
  where
    findAccountInDatabase =
        withDatabase $ \ conn -> do
          acc <- findAccountInteractive conn
          liftIO $ printAccount acc
          return acc
    readAccount account =
        Journal details . (: entries journal) . JournalEntry account
          <$> (fmap (absoluteValue . round . (*100) . unAbsoluteValue)
            <$> createTransactionAmountInteractive account)
    readEntries x =
        (liftIO . putStrLn . renderJournal . fmap unAbsoluteValue $ x)
        >> (pure . balance . map (fmap unAbsoluteValue . amount) $ entries x)
        >>= (\ currentBalance ->
                if currentBalance == 0
                    then pure x
                    else transactionInteractive x)
    entries (Journal _ xs) = xs
    amount (JournalEntry _ x) = x

emptyString :: String -> Maybe String
emptyString xs
    | all isSpace xs = Nothing
    | otherwise = Just xs

withDatabase :: (Connection -> App a) -> App a
withDatabase f = ask >>= \ cfg -> bracket
    (liftIO . open $ connectionString cfg)
    (liftIO . close)
    (\ conn -> liftIO (enableForeignKeys conn) >> f conn)
