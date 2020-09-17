{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO ()

import Data.Maybe (fromMaybe)
import Data.Char ( isSpace )
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Time (Day)
import Data.Either (isLeft)

import Control.Monad (when, forM_)
import Control.Monad.Catch ( bracket, finally )
import Control.Monad.Except
    ( MonadTrans(lift),
      MonadIO(liftIO),
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
    now <- liftIO $ today
    date <- lift $ promptDate "Date: " now
    ts <- execStateT (transactionInteractive date) []
    bracket
        (liftIO . open $ connectionString cfg)
        (liftIO . close)
        (liftIO . \ conn -> do
            withTransaction conn $ do
                forM_ ts $ \ acc -> do
                    res <- runExceptT $ saveTransaction
                        (fst acc)
                        (unAbsoluteValue <$> snd acc)
                         conn
                    when (isLeft res) $
                        error "Unexpected error when saving transaction"
        )
    return ()

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
    >>= liftEither . maybeToEither AccountNotFound

createTransactionInteractive ::
    (Accountable a) =>
    a
    -> ExceptT AccountError IO (AccountTransaction (AbsoluteValue Int))
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
    (Accountable a)
    => a
    -> ExceptT AccountError IO (TransactionAmount (AbsoluteValue Double))
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
    -> StateT [(Account, AccountTransaction (AbsoluteValue Int))] App ()
transactionInteractive date = do
    cfg <- ask
    account <- lift $ bracket
        (liftIO . open $ connectionString cfg)
        (liftIO . close)
        (lift . findAccountInteractive)
    transaction <- lift . lift $ AccountTransaction date
        <$> promptExcept "Note: " (pure . emptyString)
        <*> (fmap (absoluteValue . truncate . (*100) . unAbsoluteValue)
            <$> createTransactionAmountInteractive account)
    st <- get
    put $ (account, transaction):st

    -- output current state
    liftIO $ printJournal date $
        map (\ (x, y) -> (x, fmap unAbsoluteValue y))
        $ (account, transaction):st
    let transactions = transaction : map snd st
        transformToNum = map (fmap unAbsoluteValue . amount) transactions
        currentBalance = balance transformToNum
        in when (currentBalance /= 0) (transactionInteractive date)
  where
    emptyString xs
        | all isSpace xs = Nothing
        | otherwise = Just xs