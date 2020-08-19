module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import OptParser

main :: IO ()
main = dbSelections =<< execParser opts
  where
    opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Use double bookkeeping to handle your finances"
        <> header "Personal Finance Tracker")

dbSelections :: Options -> IO ()
dbSelections x = putStrLn $ "Filename received: " ++ dbConnection x