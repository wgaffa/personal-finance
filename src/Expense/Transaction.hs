module Expense.Transaction(
    Account(..)
    , AccountElement(..)
    , Transaction(..)
    , credit
    , debit
    , decrease
    , increase
    , zeroBalance
) where

-- Text manipulation
import qualified Data.Text as Text

-- containers
import qualified Data.Sequence as Seq

-- time and date
import Data.Time (Day)

data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Show)

data Account a = Account {
    accountName :: Text.Text
    , accountElement :: AccountElement
    , accountTransactions :: [Transaction a]
} deriving (Show)

data Transaction a = Transaction {
    transactionDate :: Day
    , transactionEntry :: TransactionEntry a
} deriving (Show)

class Accountable f where
    increase :: f -> (a -> TransactionEntry a)
    decrease :: f -> (a -> TransactionEntry a)

instance Accountable (AccountElement) where
    increase (Asset) = Debit
    increase (Liability) = Credit
    increase (Equity) = Credit
    increase (Income) = Credit
    increase (Expenses) = Debit
    decrease (Asset) = Credit
    decrease (Liability) = Debit
    decrease (Equity) = Debit
    decrease (Income) = Debit
    decrease (Expenses) = Credit

data TransactionEntry a = Debit a | Credit a
    deriving (Show, Eq)

credit :: (Num a) => a -> TransactionEntry a
credit = Credit . abs

debit :: (Num a) => a -> TransactionEntry a
debit = Debit . abs

zeroBalance :: (Eq a, Num a) => [Transaction a]-> Bool
zeroBalance xs = foldr ((+) . element . transactionEntry) 0 xs == 0
    where
        element (Debit x) = x
        element (Credit x) = negate x
