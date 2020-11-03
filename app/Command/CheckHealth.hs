module Command.CheckHealth
  ( checkHealth
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Core.App
import Core.Database

import Expense.Transaction

checkHealth :: App ()
checkHealth =
    liftIO (putStr "Checking balance: ")
    >> (withDatabase (liftIO . allTransactions) :: App [TransactionAmount Int])
    >>= liftIO . checkBalance
    >> liftIO (putStr "Checking DB version: ")
    >> withDatabase (liftIO . schemaVersion)
    >>= liftIO . checkDatabaseVersion
    >> liftIO (putStr "Checking foreign keys: ")
    >> withDatabase (liftIO . foreignKeysViolations)
    >>= liftIO . checkForeignKeys
  where
    checkBalance xs
      | zeroBalance xs = putStrLn "OK"
      | otherwise = putStrLn "NOT OK"
    checkDatabaseVersion v
      | v == latestSchemaVersion = putStrLn "OK"
      | v < latestSchemaVersion =
          putStrLn $ show v ++ " needs to updated to " ++ show latestSchemaVersion
      | v > latestSchemaVersion =
          putStrLn $ show v ++ " is newer than the latest " ++ show latestSchemaVersion ++
            ", undefined behaviour"
    checkForeignKeys violations
      | null violations = putStrLn "OK"
      | otherwise = putStrLn $ "NOT OK, found " ++ show (length violations) ++ " violations"

