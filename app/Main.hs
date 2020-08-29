module Main(main) where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Trans.Except
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

main :: IO ()
main = do
    conn <- open "db.sqlite3"
    res <- runExceptT $ createAccount conn
    case res of
        Left x -> putStrLn $ "Error: " ++ show x
        Right x -> prettyPrint x >> main
    close conn

createAccount :: Connection -> ExceptT AccountError IO Account
createAccount conn =
    createAccountInteractive
    >>= \acc -> saveAccount acc conn `catchE` (throwE . AccountNotSaved)

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = Account
    <$> promptExcept "Number: " (maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe)
    <*> promptExcept "Name: " (maybeToEither InvalidNumber . accountName . Text.pack)
    <*> promptExcept "Element: " (maybeToEither InvalidElement . readMaybe)

prettyPrint :: Account -> IO ()
prettyPrint (Account number name element) = do
    putStrLn $ (Text.unpack . unAccountName $ name) ++ " (" ++ (show . unAccountNumber $ number) ++ ") " ++ show element

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= except . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
