{-# LANGUAGE TemplateHaskell #-}

module OptParser
    ( Options(..)
    , Command(..)
    , execArgParser
    ) where

import Control.Applicative

import Options.Applicative
import Data.Semigroup ((<>))

import Development.GitRev (gitHash)
import Data.Version (showVersion)
import Paths_expense_tracker (version)

data Options = Options
    { dbConnection :: Maybe String
    , optCommand :: Command
    }
    deriving (Show)

data Command
    = List
    | CreateAccount
    | AddTransaction
    deriving (Show)

connectionOpt :: Parser (Maybe String)
connectionOpt = optional $ strOption (
    long "db-connection"
    <> help "Connection information to the database")

commands :: Parser Command
commands = hsubparser
    (  listCommand <> createCommand <> transactionCommand )
  where
    listCommand =
        command
            "list"
            (info (pure List) (progDesc "List accounts"))
    createCommand =
        command
            "create"
            (info (pure CreateAccount) (progDesc "Create a new account"))
    transactionCommand =
        command
            "add"
            (info (pure AddTransaction) (progDesc "Add a new transaction"))

options :: Parser Options
options =
    Options
    <$> connectionOpt
    <*> commands

execArgParser :: IO Options
execArgParser = execParser opts
  where
    opts = info (helper <*> versionInfo <*> options)
        ( fullDesc
        <> progDesc "Use double bookkeeping to handle your finances"
        <> header "Personal Finance Tracker")
    versionInfo = infoOption
        (showVersion version <> " " <> $(gitHash))
        (long "version" <> help "Show version")