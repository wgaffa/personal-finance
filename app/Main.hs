module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Repl
import OptParser

import Expense.Account

import Core.Utils

data AccountError = InvalidName | InvalidNumber | InvalidElement
    deriving (Show)

main :: IO ()
main = do
    createAccountInteractive

createAccountInteractive :: IO ()
createAccountInteractive = do
    number <- prompt "Number: "
    name <- prompt "Name: "
    element <- prompt "Element: "
    let account = createAccount number name element
    case account of
        Right x -> prettyPrint x
        Left x -> putStrLn $ show x

prettyPrint :: Account -> IO ()
prettyPrint (Account number name element) = do
    putStrLn $ (Text.unpack . unAccountName $ name) ++ " (" ++ (show . unAccountNumber $ number) ++ ") " ++ show element

createAccount :: String -> String -> String -> Either AccountError Account
createAccount number name element =
    Account
        <$> maybeToEither InvalidNumber readNumber
        <*> maybeToEither InvalidName (accountName (Text.pack name))
        <*> maybeToEither InvalidElement (readMaybe element)
  where
    readNumber = readMaybe number >>= accountNumber

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
