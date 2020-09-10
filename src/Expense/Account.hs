module Expense.Account
    ( Ledger(..)
    , Account(..)
    , AccountElement(..)
    , AccountName()
    , AccountNumber()
    , AccountTransaction(..)
    , accountBalance
    , accountName
    , accountNumber
    , emptyAccountNumber
    , decrease
    , increase
    , transactionType
    , ledgerTransaction
    , unAccountName
    , unAccountNumber
    ) where

import Data.Char
import qualified Data.Text as Text

import Data.Time (Day)

import Expense.Transaction

data Ledger a = Ledger Account [AccountTransaction a]
    deriving(Show)

-- | All different account elements (types)
data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show, Read)

-- | A string that contains atleast one printable character
newtype AccountName = AccountName { unAccountName :: Text.Text }
    deriving (Ord, Eq, Show)

-- | Smart constructor for 'AccountName'
accountName :: Text.Text -> Maybe AccountName
accountName name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ AccountName name

-- | An account number is any positive number
newtype AccountNumber = AccountNumber { unAccountNumber :: Int }
    deriving (Ord, Eq, Show)

emptyAccountNumber :: AccountNumber
emptyAccountNumber = AccountNumber 0

-- | Smart constructor for 'AccountNumber'
accountNumber :: Int -> Maybe AccountNumber
accountNumber number
    | number > 0 = Just $ AccountNumber number
    | otherwise = Nothing

-- | Account structure
data Account = Account {
    number :: AccountNumber
    , name :: AccountName
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
    , description :: Maybe String -- ^ Description for the transaction
    , amount :: TransactionAmount a -- ^ Amount debited or credited
} deriving (Show)

ledgerTransaction :: AccountTransaction a -> Ledger a -> Ledger a
ledgerTransaction transaction (Ledger account transactions) =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [transaction]

accountBalance :: (Num a) => Ledger a -> TransactionAmount a
accountBalance (Ledger acc ts) =
    flip TransactionAmount (balance ts)
    . transactionType
    . element $ acc
  where
    balance = foldr ((+) . toNumeral . amount) 0
