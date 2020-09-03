{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Database.SQLite.Simple

import Repl
import OptParser
import Core.Database
import Core.PrettyPrint

import Expense.Account

import Core.Utils
import Core.Error

data AppEnvironment = AppEnvironment
    { connectionString :: String
    , command :: Command
    } deriving (Show)

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

readEnvironment :: IO AppEnvironment
readEnvironment = do
    (Options {..}) <- execArgParser
    return AppEnvironment {
        connectionString = maybe "db.sqlite3" id dbConnection
        , command = optCommand
        }

showAccounts :: App ()
showAccounts = do
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    accounts <- liftIO $ allAccounts conn
    liftIO $ printListAccounts accounts

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

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: "
        (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: "
        (maybeToEither InvalidNumber . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)
