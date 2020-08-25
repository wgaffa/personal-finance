module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Repl
import OptParser

import Expense.Account

import Core.Utils

data AccountError = InvalidName | InvalidNumber | InvalidElement
    deriving (Show)

main :: IO ()
main = do
    res <- runExceptT createAccountInteractive
    case res of
        Left x -> putStrLn $ "Error: " ++ show x
        Right x -> prettyPrint x >> main

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
