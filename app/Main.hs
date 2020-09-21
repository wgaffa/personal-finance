{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO ()

import Data.Maybe (fromMaybe)
import Data.Char ( isSpace )
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Time (Day)
import Data.Either (isLeft)

import Control.Monad (when, forM_, forM)
import Control.Monad.Catch
    ( MonadThrow, MonadCatch, MonadMask
    , bracket, finally)
import Control.Monad.Except
    ( MonadTrans(lift),
      MonadIO(liftIO),
      MonadError(),
      ExceptT,
      liftEither,
      runExceptT )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Control.Monad.Trans.State (execStateT, execState, put, modify, StateT, get)
import Control.Monad.Reader
    ( MonadTrans(lift),
      MonadIO(liftIO),
      ReaderT(runReaderT),
      MonadReader(ask) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Database.SQLite.Simple (withTransaction,  close, open, Connection )
import Database.SQLite.Simple.FromField ( FromField )

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

readEnvironment :: IO AppEnvironment
readEnvironment = do
    Options {..} <- execArgParser
    return AppEnvironment {
        connectionString = fromMaybe "db.sqlite3" dbConnection
        , command = optCommand
        }

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

showTransactions :: ShowOptions -> App ()
showTransactions ShowOptions{..} =
    (fmap unAbsoluteValue
        <$> ((liftEither
                . maybeToEither InvalidNumber
                . accountNumber $ filterAccount)
            >>= (\ accountNumber ->
                withDatabase (liftIO . findLedger accountNumber)
            )))
    >>= liftIO . printLedger

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
    now <- liftIO $ today
    date <- promptDate "Date: " now
    ts <- transactionInteractive date []
    withDatabase $ liftIO . \ conn -> do
        withTransaction conn $ do
            forM_ ts $ \ acc -> do
                res <- runExceptT $ saveTransaction
                    (fst acc)
                    (unAbsoluteValue <$> snd acc)
                     conn
                when (isLeft res) $
                    error "Unexpected error when saving transaction"
    return ()

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

createTransactionInteractive ::
    (Accountable a, MonadError AccountError m, MonadIO m) =>
    a
    -> m (AccountTransaction (AbsoluteValue Int))
createTransactionInteractive x =
    AccountTransaction
    <$> promptExcept "Date: "
        (maybeToEither ParseError . readMaybe)
    <*> promptExcept "Description: "
        (pure . emptyString)
    <*> (fmap (absoluteValue . truncate . (*100) . unAbsoluteValue)
        <$> createTransactionAmountInteractive x)
  where
    emptyString xs
        | all isSpace xs = Nothing
        | otherwise = Just xs

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
    Day
    -> [(Account, AccountTransaction (AbsoluteValue Int))]
    -> App [(Account, AccountTransaction (AbsoluteValue Int))]
transactionInteractive date accu =
    findAccountInDatabase >>= readAccount >>= readEntries
  where
    findAccountInDatabase =
        withDatabase findAccountInteractive
    readAccount account =
        (AccountTransaction date
            <$> promptExcept "Note: " (pure . emptyString)
            <*> (fmap (absoluteValue . truncate . (*100) . unAbsoluteValue)
                <$> createTransactionAmountInteractive account))
        >>= \ x -> pure $ (account, x):accu
    readEntries entries =
        (liftIO $ printJournal date
            $ map (\ (x, y) -> (x, fmap unAbsoluteValue y)) entries)
        >> (pure . balance . map (fmap unAbsoluteValue . amount . snd) $ entries)
        >>= (\ currentBalance ->
                if currentBalance == 0
                    then pure entries
                    else transactionInteractive date entries)
    emptyString xs
        | all isSpace xs = Nothing
        | otherwise = Just xs

withDatabase :: (Connection -> App a) -> App a
withDatabase f = ask >>= \ cfg -> bracket 
    (liftIO . open $ connectionString cfg)
    (liftIO . close)
    (f)
