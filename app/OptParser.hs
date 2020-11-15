{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module OptParser (
    Options (..),
    Command (..),
    ShowOptions (..),
    execArgParser,
) where

import Control.Applicative (optional)

import Options.Applicative (
    Parser,
    argument,
    auto,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    metavar,
    progDesc,
    strOption,
    switch,
 )

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_expense_tracker (version)

-- | The commandline arguments for the application
data Options = Options
    { -- |The file or connection to use for the database
      dbConnection :: Maybe String
    , -- |The command we wish to run
      optCommand :: Command
    }

-- | Type that holds the arguments of the command /show/
data ShowOptions = ShowOptions
    { -- |The account number to show
      filterAccount :: Int
    , showId :: Bool
    }

{- | Different commands that can be passed to the application and
 their arguments if used
-}
data Command
    = List
    | CreateAccount
    | AddTransaction
    | ShowAccount ShowOptions
    | UpdateDatabase
    | CheckHealth
    | AccountingPeriod

connectionOpt :: Parser (Maybe String)
connectionOpt =
    optional $
        strOption
            ( long "db-connection"
                <> help "Connection information to the database"
            )

commands :: Parser Command
commands =
    hsubparser
        ( listCommand <> createCommand
            <> transactionCommand
            <> showCommand
            <> updateCommand
            <> checkHealthCommand
            <> showAccountingPeriod
        )
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
            ( info
                (ShowAccount <$> showOptions)
                (progDesc "Show transaction for an account")
            )
    updateCommand =
        command
            "update-db"
            ( info
                (pure UpdateDatabase)
                (progDesc "Update the database to latest version")
            )
    checkHealthCommand =
        command
            "checkhealth"
            ( info
                (pure CheckHealth)
                (progDesc "Check the health of the application and database")
            )
    showAccountingPeriod =
        command
            "period"
            ( info
                (pure AccountingPeriod)
                (progDesc "Show the current accounting period")
            )

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

-- | Runs the parser and returns an 'Options' type
execArgParser :: IO Options
execArgParser = execParser opts
  where
    opts =
        info
            (helper <*> versionInfo <*> options)
            ( fullDesc
                <> progDesc "Use double bookkeeping to handle your finances"
                <> header "Personal Finance Tracker"
            )
    versionInfo =
        infoOption
            (showVersion version <> " " <> $(gitHash))
            (long "version" <> help "Show version")
