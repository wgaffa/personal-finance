module OptParser
    ( Options(..)
    , execArgParser
    ) where

import Control.Applicative

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { dbConnection :: Maybe String }
    deriving (Show)

connectionOpt :: Parser (Maybe String)
connectionOpt = optional $ strOption (
    long "db-connection"
    <> help "Connection information to the database")

options :: Parser Options
options =
    Options
    <$> connectionOpt

execArgParser :: IO Options
execArgParser = execParser opts
  where
    opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Use double bookkeeping to handle your finances"
        <> header "Personal Finance Tracker")
