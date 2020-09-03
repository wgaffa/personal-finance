module Core.Utils
    ( maybeToEither
    , prompt
    , promptExcept
    ) where

import System.IO

import Control.Monad.Except

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x

promptExcept :: String -> (String -> Either e a) -> ExceptT e IO a
promptExcept text f =
    (liftIO . prompt $ text)
    >>= liftEither . f

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
