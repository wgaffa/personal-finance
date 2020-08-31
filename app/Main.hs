{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Catch
import Control.Monad.Trans.Except
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

data AccountError
    = InvalidName
    | InvalidNumber
    | InvalidElement
    | AccountNotSaved String
    deriving (Show)

data AppEnvironment = AppEnvironment
    { connectionString :: String
    } deriving (Show)

type App = ReaderT AppEnvironment (ExceptT AccountError IO)

defaultConfig :: AppEnvironment
defaultConfig = AppEnvironment
    { connectionString = "db.sqlite3" }

main :: IO ()
main = do
    env <- readEnvironment
    res <- runExceptT (runReaderT createAccount env)
    case res of
        Left x -> putStrLn $ "Error: " ++ show x
        Right x -> putStr "Saved account: "
            >> printAccount x
            >> putChar '\n'
            >> main

readEnvironment :: IO AppEnvironment
readEnvironment = do
    (Options {..}) <- execArgParser
    return defaultConfig {
        connectionString = maybe "db.sqlite3" id dbConnection
        }

createAccount :: ReaderT AppEnvironment (ExceptT AccountError IO) Account
createAccount = do
    acc <- lift $ createAccountInteractive
    liftIO $ putStrLn "Attempting to save account"
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    lift $ finally (save acc conn) (liftIO $ closeConn conn)
    return acc
  where
      save acc conn = saveAccount acc conn `catchE` (throwE . AccountNotSaved)

closeConn :: Connection -> IO ()
closeConn conn = putStrLn "Closing connection" >> close conn

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: " (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: " (maybeToEither InvalidNumber . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= except . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
