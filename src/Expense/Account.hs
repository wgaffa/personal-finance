module Expense.Account
    ( Ledger(..)
    , Account(..)
    , AccountElement(..)
    , AccountName()
    , AccountTransaction(..)
    , accountName
    , decrease
    , increase
    , transactionType
    , ledgerTransaction
    ) where

import Data.Char
import qualified Data.Text as Text

import Data.Time (Day)

import Expense.Transaction

data Ledger a = Ledger Account [AccountTransaction a]
    deriving(Show)

-- | All different account elements (types)
data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show)

-- | A string that contains atleast one printable character
newtype AccountName = AccountName { unAccountName :: Text.Text }
    deriving (Ord, Eq, Show)

-- | Constructor for 'AccountName'
accountName :: Text.Text -> Maybe AccountName
accountName name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ AccountName name

-- | Account structure
data Account = Account {
    name :: AccountName
    , element :: AccountElement
} deriving (Ord, Eq, Show)

class Accountable f where
    transactionType :: f -> TransactionType
    increase :: f -> (a -> TransactionAmount a)
    decrease :: f -> (a -> TransactionAmount a)

instance Accountable Account where
    transactionType = transactionType . element
    increase = increase . element
    decrease = increase . element

debitAccounts :: [AccountElement]
debitAccounts = [Asset, Expenses]
instance Accountable AccountElement where
    increase x
        | x `elem` debitAccounts = debit
        | otherwise = credit
    decrease x
        | x `elem` debitAccounts = credit
        | otherwise = debit
    transactionType x
        | x `elem` debitAccounts = Debit
        | otherwise = Credit

-- | Account specific transaction that goes in to a ledger
data AccountTransaction a = AccountTransaction {
    date :: Day -- ^ Date of the transaction
    , amount :: TransactionAmount a -- ^ Amount debited or credited
} deriving (Show)

ledgerTransaction :: AccountTransaction a -> Ledger a -> Ledger a
ledgerTransaction transaction (Ledger account transactions) =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [transaction]
