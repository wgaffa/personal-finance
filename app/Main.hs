module Main where

import System.IO

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Repl
import OptParser

import Expense.Account

main :: IO ()
main = do
    createAccountInteractive

createAccountInteractive :: IO ()
createAccountInteractive = do
    number <- prompt "Number: "
    name <- prompt "Name: "
    element <- prompt "Element: "
    let account = createAccount number name element
        in print account

createAccount :: String -> String -> String -> Maybe Account
createAccount number name element =
    Account
    <$> accountNumber (read number)
    <*> accountName (Text.pack name)
    <*> readMaybe element

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine