module Core.Utils
    ( maybeToEither
    ) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither l Nothing = Left l
maybeToEither _ (Just x) = Right x