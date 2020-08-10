module Expense.Transaction(
    Account(..)
    , AccountElement(..)
    , AccountTransaction(..)
    , AccountTypeable(..)
    , BalanceAmount(..)
    , BalanceSheet
    , Ledger(..)
    , Transaction(..)
    , TransactionEntry(..)
    , TransactionType(..)
    , credit
    , debit
    , decrease
    , increase
    , printableString
    , toNumeral
    , toSigNum
    , transactionEntry
    , zeroBalance
) where

-- Text manipulation
import Data.Char
import qualified Data.Text as Text

-- containers
import qualified Data.Sequence as Seq

-- time and date
import Data.Time (Day)

class Accountable f where
    increase :: (Num a) => f -> (a -> TransactionAmount a)
    decrease :: (Num a) => f -> (a -> TransactionAmount a)

instance Accountable (Account a) where
    increase = increase . accountElement
    decrease = increase . accountElement

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

data TransactionType = Debit | Credit
    deriving (Eq, Enum, Bounded, Show)

data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show)

newtype PrintableString = PrintableString { unPrintableString :: Text.Text }
    deriving (Ord, Eq, Show)

data Account a = Account {
    accountName :: PrintableString
    , accountElement :: AccountElement
} deriving (Ord, Eq, Show)

data TransactionEntry a = TransactionEntry (Account a) (TransactionAmount a)
    deriving (Show)

data Transaction a = Transaction {
    transactionDate :: Day
    , transactionEntries :: [TransactionEntry a]
} deriving (Show)

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

transactionEntry :: (Num a) => (Account a -> a -> TransactionAmount a) -> Account a -> a -> TransactionEntry a
transactionEntry f account = TransactionEntry account . f account

-- experimental below
type BalanceSheet a = AccountElement -> [Ledger a] -> BalanceAmount a
type ZeroBalance a = [TransactionAmount a] -> Bool

data BalanceAmount a = BalanceAmount TransactionType a
    deriving (Show)

data Ledger a = Ledger (Account a) [AccountTransaction a]
    deriving(Show)

data AccountTransaction a = AccountTransaction {
    atDate :: Day
    , atAmount :: TransactionAmount a
} deriving (Show)

class AccountTypeable f where
    transactionType :: f -> TransactionType
    toBalance :: (Num a) => f -> a -> BalanceAmount a
    toBalance l x = BalanceAmount (transactionType l) $ toSigNum (transactionType l) * x

instance AccountTypeable AccountElement where
    transactionType Asset     = Debit
    transactionType Liability = Credit
    transactionType Equity    = Credit
    transactionType Income    = Credit
    transactionType Expenses  = Debit

zeroBalance :: (Eq a, Num a) => [TransactionAmount a]-> Bool
zeroBalance xs = foldr ((+) . toNumeral) 0 xs == 0

toNumeral :: (Num a) => TransactionAmount a -> a
toNumeral (TransactionAmount t x) = toSigNum t * x

toSigNum :: (Num a) => TransactionType -> a
toSigNum Debit = 1
toSigNum Credit = -1
