module Core.Prompt
    ( prompt
    , promptDate
    , promptExcept
    ) where

import System.IO (hFlush, stdout)

import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.Time (Day)

import Control.Monad.Except

import Core.Error
import Core.Utils

promptDate :: String -> Day -> ExceptT AccountError IO Day
promptDate text date =
    (liftIO . prompt $ text)
    >>= \ xs -> if (all isSpace xs)
                then return date
                else liftEither $ maybeToEither ParseError . readMaybe  $ xs

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= liftEither . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
