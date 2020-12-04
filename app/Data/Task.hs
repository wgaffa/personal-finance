module Data.Task (
  -- * Types
  Task (..),
  TaskStatus (..),

  -- * Task constructors
  print,
  printLn,
  task,
) where

import Prelude hiding (print)

import Control.Monad.Free (Free (..), liftF)

import Core.App

-- | This is the return value for most tasks
data TaskStatus
  = -- | Return if everything is ok
    Ok
  | -- | Return when if we are in a state that undefined behaviour may occur
    Error String
  | -- | Return when task is in a state it can still function somewhat
    Warning String

data Task a next
  = Task (App a) (a -> next)
  | Print String next

instance Functor (Task a) where
  fmap f (Task x g) = Task x (f . g)
  fmap f (Print x next) = Print x (f next)

print :: String -> Free (Task a) ()
print x = liftF $ Print x ()

printLn :: String -> Free (Task a) ()
printLn x = liftF $ Print (x ++ "\n") ()

task :: App a -> Free (Task a) a
task x = liftF $ Task x id
