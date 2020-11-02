{-# LANGUAGE TemplateHaskell #-}

module OptParser
    ( Options(..)
    , Command(..)
    , ShowOptions(..)
    , execArgParser
    ) where

import Control.Applicative ( optional )

import Options.Applicative
    ( optional,
      switch,
      argument,
      auto,
      command,
      fullDesc,
      header,
      help,
      info,
      infoOption,
      long,
      metavar,
      progDesc,
      strOption,
      execParser,
      helper,
      hsubparser,
      Parser )
import Data.Semigroup ((<>))

import Development.GitRev (gitHash)
import Data.Version (showVersion)
import Paths_expense_tracker (version)

data Options = Options
    { dbConnection :: Maybe String
    , optCommand :: Command
    }

data ShowOptions = ShowOptions
    { filterAccount :: Int
    , showId :: Bool
    }

data Command
    = List
    | CreateAccount
    | AddTransaction
    | ShowAccount ShowOptions
    | UpdateDatabase
    | CheckHealth

connectionOpt :: Parser (Maybe String)
connectionOpt = optional $ strOption (
    long "db-connection"
    <> help "Connection information to the database")

commands :: Parser Command
commands = hsubparser
    (  listCommand <> createCommand
        <> transactionCommand <> showCommand
        <> updateCommand <> checkHealthCommand)
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
    showCommand =
        command
            "show"
            (info
                (ShowAccount <$> showOptions)
                (progDesc "Show transaction for an account")
            )
    updateCommand =
        command
            "update-db"
            (info
                (pure UpdateDatabase)
                (progDesc "Update the database to latest version"))
    checkHealthCommand =
        command
            "checkhealth"
            (info
                (pure CheckHealth)
                (progDesc "Check the health of the application and database"))

showOptions :: Parser ShowOptions
showOptions =
    ShowOptions
    <$> argument auto (metavar "ACCOUNTID")
    <*> switch
      (long "show-uuid" <> help "Show UUID")

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
