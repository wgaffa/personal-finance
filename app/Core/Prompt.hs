{-# LANGUAGE FlexibleContexts #-}

module Core.Prompt (
    prompt,
    promptDate,
    promptExcept,
) where

import System.IO (hFlush, stdout)

import Data.Char (isSpace)
import Data.Time (Day)
import Text.Read (readMaybe)

import Control.Monad.Except

import Core.Error
import Core.Utils

promptDate ::
    (MonadError AccountError m, MonadIO m) =>
    String ->
    Day ->
    m Day
promptDate text date =
    (liftIO . prompt $ text)
        >>= \xs ->
            if all isSpace xs
                then return date
                else liftEither $ maybeToEither ParseError . readMaybe $ xs

promptExcept ::
    (MonadError e m, MonadIO m) =>
    String ->
    (String -> Either e a) ->
    m a
promptExcept text f =
    (liftIO . prompt $ text)
        >>= liftEither . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
