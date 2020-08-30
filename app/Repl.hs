module Repl
    ( repl
    ) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Console.Repline

import Data.List (isPrefixOf, unwords)

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ print input

completer :: Monad m => WordCompleter m
completer n = do
    let commands = ["account", "transaction"]
    return $ filter (isPrefixOf n) commands

-- commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

quit :: String -> Repl ()
quit _ = abort

opts :: [(String, String -> Repl ())]
opts =
    [ ("quit", quit)
    ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

final :: Repl ExitDecision
final = do
    liftIO $ putStrLn "Goodbye!"
    return Exit

repl :: IO ()
repl = evalRepl (const $ pure ">>> ") cmd opts (Just ':') (Just "paste") (Word0 completer) ini final