module Core.Error
    ( AccountError(..)
    ) where

data AccountError
    = InvalidName
    | InvalidNumber
    | InvalidElement
    | AccountNotSaved String
    | MiscError String

instance Show AccountError where
    show InvalidName = "account name is not valid"
    show InvalidNumber = "account number must be positive"
    show InvalidElement = "could not find account element"
    show (AccountNotSaved s) = "could not save account, reason: " ++ s
