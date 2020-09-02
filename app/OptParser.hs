module OptParser
    ( Options(..)
    , Command(..)
    , execArgParser
    ) where

import Control.Applicative

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { dbConnection :: Maybe String
    , optCommand :: Command
    }
    deriving (Show)

data Command
    = List
    | CreateAccount
    deriving (Show)

connectionOpt :: Parser (Maybe String)
connectionOpt = optional $ strOption (
    long "db-connection"
    <> help "Connection information to the database")

commands :: Parser Command
commands = hsubparser
    (  listCommand <> createCommand )
  where
    listCommand =
        command
            "list"
            (info listOptions (progDesc "List accounts"))
    createCommand =
        command
            "create"
            (info (pure CreateAccount) (progDesc "Create a new account"))
    listOptions = pure List

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
    versionInfo = infoOption "0.1.0.0" (long "version" <> help "Show version")