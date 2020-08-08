module Expense.Transaction(
    Account(..)
    , AccountElement(..)
    , Transaction(..)
    , credit
    , debit
    , decrease
    , increase
    , printableString
    , zeroBalance
) where

-- Text manipulation
import Data.Char
import qualified Data.Text as Text

-- containers
import qualified Data.Sequence as Seq

-- time and date
import Data.Time (Day)

data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Show)

newtype PrintableString = PrintableString { unPrintableString :: Text.Text }
    deriving (Show)

data Account a = Account {
    accountName :: PrintableString
    , accountElement :: AccountElement
} deriving (Show)

data Transaction a = Transaction {
    transactionDate :: Day
    , transactionAmount :: TransactionAmount a
} deriving (Show)

class Accountable f where
    increase :: f -> (a -> TransactionAmount a)
    decrease :: f -> (a -> TransactionAmount a)

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

data TransactionAmount a = Debit a | Credit a
    deriving (Show, Eq)

printableString :: Text.Text -> Maybe (PrintableString)
printableString name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ PrintableString name

credit :: (Num a) => a -> TransactionAmount a
credit = Credit . abs

debit :: (Num a) => a -> TransactionAmount a
debit = Debit . abs

zeroBalance :: (Eq a, Num a) => [Transaction a]-> Bool
zeroBalance xs = foldr ((+) . element . transactionAmount) 0 xs == 0
    where
        element (Debit x) = x
        element (Credit x) = negate x
