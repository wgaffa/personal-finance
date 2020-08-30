{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Database.SQLite.Simple

import Repl
import OptParser
import Core.Database

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

-- newtype App a = App
--     { runApp :: ReaderT AppEnvironment (ExceptT AccountError IO) a }
--     deriving (Monad, MonadIO, MonadReader AppEnvironment)

defaultConfig :: AppEnvironment
defaultConfig = AppEnvironment
    { connectionString = "db.sqlite3" }

main :: IO ()
main = do
    res <- runExceptT (runReaderT createAccount defaultConfig)
    case res of
        Left x -> putStrLn $ "Error: " ++ show x
        Right x -> putStrLn ("Saved account: " ++ prettyPrint x) >> main

createAccount :: ReaderT AppEnvironment (ExceptT AccountError IO) Account
createAccount = do
    acc <- lift $ createAccountInteractive
    liftIO $ putStrLn "Attempting to save account"
    cfg <- ask
    conn <- liftIO . open $ connectionString cfg
    lift $ saveAccount acc conn `catchE` (throwE . AccountNotSaved)
    liftIO $ close conn
    return acc

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: " (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: " (maybeToEither InvalidNumber . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

prettyPrint :: Account -> String
prettyPrint (Account number name element) = do
    (Text.unpack . unAccountName $ name) ++ " (" ++ (show . unAccountNumber $ number) ++ ") " ++ show element

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= except . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
