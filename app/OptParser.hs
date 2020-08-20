module OptParser
    ( Options()
    , execArgParser
    , dbConnection
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { dbConnection :: String }

options :: Parser Options
options =
    Options
    <$> argument str (metavar "FILE")

execArgParser :: IO ()
execArgParser = dbConnectionArg =<< execParser opts
  where
    opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Use double bookkeeping to handle your finances"
        <> header "Personal Finance Tracker")

dbConnectionArg :: Options -> IO ()
dbConnectionArg x = putStrLn $ "Filename received: " ++ dbConnection x