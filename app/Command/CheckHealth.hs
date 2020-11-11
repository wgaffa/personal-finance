{-# OPTIONS_GHC -Wno-orphans #-}

module Command.CheckHealth
  (
  -- * Default tasks
  checkHealth
  -- * Interpreters
  , runTasks
  ) where

import Prelude hiding (print)

import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Core.App
import Core.Database
import Data.Task

import Expense.Transaction

instance Show TaskStatus where
    show Ok = "OK"
    show (Error s) = "ERROR: " ++ s
    show (Warning s) = "WARN: " ++ s

checkHealth :: Free (Task TaskStatus) ()
checkHealth =
    printLn "Checking transactions"
    >> print "Balance: " >> task taskBalance >>= printLn . show
    >> printLn "Checking database integrity"
    >> print "DB version: " >> task taskDatabaseVersion >>= printLn . show
    >> print "Foreign keys: " >> task taskForeignKeys >>= printLn . show

runTasks :: (Show a) => Free (Task a) r -> App r
runTasks (Free (Task action g)) = action >>= runTasks . g
runTasks (Free (Print s next)) = liftIO (putStr s) >> runTasks next
runTasks (Pure r) = return r

taskBalance :: App TaskStatus
taskBalance =
    (withDatabase (liftIO . allTransactions) :: App [TransactionAmount Int])
    >>= validateBalance
  where
    validateBalance xs
      | zeroBalance xs = return Ok
      | otherwise = return $ Error mempty

taskDatabaseVersion :: App TaskStatus
taskDatabaseVersion =
    withDatabase (liftIO . schemaVersion)
    >>= validateDatabaseVersion
  where
    validateDatabaseVersion v
      | v == latestSchemaVersion = return Ok
      | v < latestSchemaVersion =
          return . Warning $ show v ++ " needs to updated to " ++ show latestSchemaVersion
      | v > latestSchemaVersion =
          return . Error $ show v ++ " is newer than the latest " ++ show latestSchemaVersion ++
            ", undefined behaviour"
      | otherwise = error "unreachable code, if you see this please report this"

taskForeignKeys :: App TaskStatus
taskForeignKeys =
    withDatabase (liftIO . foreignKeysViolations)
    >>= validateForeignKeys
  where
    validateForeignKeys violations
      | null violations = return Ok
      | otherwise = return . Warning $ "Found " ++ show (length violations) ++ " violations"

