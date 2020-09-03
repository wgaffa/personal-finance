module Core.Error
    ( AccountError(..)
    ) where

data AccountError
    = InvalidName
    | InvalidNumber
    | InvalidElement
    | AccountNotSaved String
    deriving (Show)
