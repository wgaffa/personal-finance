module Core.Error
    ( AccountError(..)
    ) where

data AccountError
    = InvalidName
    | InvalidNumber
    | InvalidElement
    | AccountNotSaved String
    | InvalidTransactionType
    | ParseError
    | MiscError String

instance Show AccountError where
    show InvalidName = "name is not valid"
    show InvalidNumber = "number must be positive"
    show InvalidElement = "could not find account element"
    show (AccountNotSaved s) = "could not save account, reason: " ++ s
    show (InvalidTransactionType) = "the type is not valid"
    show (ParseError) = "could not parse input"
    show (MiscError s) = "error: " ++ s
