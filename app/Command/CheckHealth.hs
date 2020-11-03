module Command.CheckHealth
  ( checkHealth
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Core.App
import Core.Database

import Expense.Transaction

checkHealth :: App ()
checkHealth =
    checkBalance >> checkDatabaseVersion >> checkForeignKeys
  where
    checkBalance =
        liftIO (putStr "Checking balance: ")
        >> (withDatabase (liftIO . allTransactions) :: App [TransactionAmount Int])
        >>= liftIO . validateBalance
    validateBalance xs
      | zeroBalance xs = putStrLn "OK"
      | otherwise = putStrLn "NOT OK"
    checkDatabaseVersion =
        liftIO (putStr "Checking DB version: ")
        >> withDatabase (liftIO . schemaVersion)
        >>= liftIO . validateDatabaseVersion
    validateDatabaseVersion v
      | v == latestSchemaVersion = putStrLn "OK"
      | v < latestSchemaVersion =
          putStrLn $ show v ++ " needs to updated to " ++ show latestSchemaVersion
      | v > latestSchemaVersion =
          putStrLn $ show v ++ " is newer than the latest " ++ show latestSchemaVersion ++
            ", undefined behaviour"
    checkForeignKeys =
        liftIO (putStr "Checking foreign keys: ")
        >> withDatabase (liftIO . foreignKeysViolations)
        >>= liftIO . validateForeignKeys
    validateForeignKeys violations
      | null violations = putStrLn "OK"
      | otherwise = putStrLn $ "NOT OK, found " ++ show (length violations) ++ " violations"

