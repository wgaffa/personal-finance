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
        Right x -> print x >> main

createAccountInteractive :: ExceptT AccountError IO Account
createAccountInteractive = do
    number <- liftIO . prompt $ "Number: "
    name <- liftIO . prompt $ "Name: "
    element <- liftIO . prompt $ "Element: "
    -- return ()
    let account = createAccount number name element
    case account of
        Right x -> ExceptT . return $ Right x
        Left x ->  ExceptT . return $ Left x

prettyPrint :: Account -> IO ()
prettyPrint (Account number name element) = do
    putStrLn $ (Text.unpack . unAccountName $ name) ++ " (" ++ (show . unAccountNumber $ number) ++ ") " ++ show element

promptAccount :: ExceptT AccountError IO Account
promptAccount = Account
    <$> promptNumber
    <*> promptName
    <*> promptElement

promptName :: ExceptT AccountError IO AccountName
promptName =
    (liftIO . prompt $ "Name: ")
    >>= except . maybeToEither InvalidName . accountName . Text.pack

promptNumber :: ExceptT AccountError IO AccountNumber
promptNumber =
    (liftIO . prompt $ "Number: ")
    >>= except . maybeToEither InvalidNumber . (=<<) accountNumber . readMaybe

promptElement :: ExceptT AccountError IO AccountElement
promptElement =
    (liftIO . prompt $ "Element: ")
    >>= except . maybeToEither InvalidElement . readMaybe

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
