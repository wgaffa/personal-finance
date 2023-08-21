{-# LANGUAGE FlexibleContexts #-}

module Core.Prompt (
    prompt,
    promptDate,
    promptExcept,
) where

import System.Console.Haskeline

import Data.Char (isSpace)
import Data.Maybe
import Data.Time (Day)
import Text.Read (readMaybe)

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Control.Monad.Trans ()

import Core.Error
import Utils.Maybe

promptDate ::
    (MonadError AccountError m, MonadIO m, MonadMask m) =>
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
    (MonadError e m, MonadIO m, MonadMask m) =>
    String ->
    (String -> Either e a) ->
    m a
promptExcept text f =
    (liftIO . prompt $ text)
        >>= liftEither . f

prompt :: (MonadIO m, MonadMask m) => String -> m String
prompt text = runInputT defaultSettings (getInputLine text) >>= return . fromMaybe mempty
