module Expense.Transaction(
    Account(..)
    , AccountElement(..)
    , AccountTransaction(..)
    , AccountTypeable(..)
    , BalanceAmount(..)
    , BalanceSheet
    , Ledger(..)
    , TransactionType(..)
    , credit
    , debit
    , decrease
    , increase
    , printableString
    , toNumeral
    , toSigNum
    , zeroBalance
) where

-- Text manipulation
import Data.Char
import qualified Data.Text as Text

-- containers
import qualified Data.Sequence as Seq

-- time and date
import Data.Time (Day)

type BalanceSheet a = AccountElement -> [Ledger a] -> BalanceAmount a
type ZeroBalance a = [TransactionAmount a] -> Bool

data TransactionType = Debit | Credit
    deriving (Eq, Enum, Bounded, Show)
data BalanceAmount a = BalanceAmount TransactionType a
    deriving (Show)

data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show)

newtype PrintableString = PrintableString { unPrintableString :: Text.Text }
    deriving (Ord, Eq, Show)

data Account a = Account {
    accountName :: PrintableString
    , accountElement :: AccountElement
} deriving (Ord, Eq, Show)

data Ledger a = Ledger (Account a) [AccountTransaction a]
    deriving(Show)

data TransactionEntry a = TransactionEntry (Account a) (TransactionAmount a)

data Transaction a = Transaction {
    transactionDate :: Day
    , transactionEntries :: [TransactionEntry a]
}

data AccountTransaction a = AccountTransaction {
    atDate :: Day
    , atAmount :: TransactionAmount a
} deriving (Show)

class AccountTypeable f where
    transactionType :: f -> TransactionType
    toBalance :: (Num a) => f -> a -> BalanceAmount a
    toBalance l x = BalanceAmount (transactionType l) $ toSigNum (transactionType l) * x

class Accountable f where
    increase :: (Num a) => f -> (a -> TransactionAmount a)
    decrease :: (Num a) => f -> (a -> TransactionAmount a)

instance AccountTypeable AccountElement where
    transactionType Asset     = Debit
    transactionType Liability = Credit
    transactionType Equity    = Credit
    transactionType Income    = Credit
    transactionType Expenses  = Debit

instance Accountable AccountElement where
    increase Asset     = debit
    increase Liability = credit
    increase Equity    = credit
    increase Income    = credit
    increase Expenses  = debit
    decrease Asset     = credit
    decrease Liability = debit
    decrease Equity    = debit
    decrease Income    = debit
    decrease Expenses  = credit

data TransactionAmount a = TransactionAmount TransactionType a
    deriving (Show, Eq)

printableString :: Text.Text -> Maybe PrintableString
printableString name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ PrintableString name

credit :: (Num a) => a -> TransactionAmount a
credit = TransactionAmount Credit . abs

debit :: (Num a) => a -> TransactionAmount a
debit = TransactionAmount Debit . abs

zeroBalance :: (Eq a, Num a) => [AccountTransaction a]-> Bool
zeroBalance xs = foldr ((+) . toNumeral . atAmount) 0 xs == 0

toNumeral :: (Num a) => TransactionAmount a -> a
toNumeral (TransactionAmount t x) = toSigNum t * x

toSigNum :: (Num a) => TransactionType -> a
toSigNum Debit = 1
toSigNum Credit = -1