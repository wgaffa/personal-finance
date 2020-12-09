{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module OptParser (
    Options (..),
    Command (..),
    ShowOptions (..),
    execArgParser,
    lastOpt,
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
    showDefault,
    str,
    strOption,
    switch,
    value,
 )

import Data.Monoid (Last (Last))
import Data.Version (showVersion)

import Development.GitRev (gitHash)
import Paths_expense_tracker (version)

import Core.App (Command (..), ShowOptions (..))
import Core.Config (Build (..), Config (..))

-- | The commandline arguments for the application
data Options = Options
    { -- |Configuration for the application
      config :: Config Build
    , -- |The command we wish to run
      optCommand :: Command
    }

lastOpt :: Parser a -> Parser (Build Last a)
lastOpt parser = Build . Last <$> optional parser

configOpt :: Parser (Config Build)
configOpt =
    Config
        <$> lastOpt
            ( strOption
                ( long "db-connection"
                    <> help "Connection information to the database"
                    <> value "db.sqlite3"
                    <> showDefault
                )
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
            <> newAccountingPeriod
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
    newAccountingPeriod =
        command
            "new-period"
            ( info
                (NewAccountingPeriod <$> argument str (metavar "NAME"))
                (progDesc "Close current accounting period and open a new one")
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
        <$> configOpt
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
