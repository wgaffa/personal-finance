module Expense.Transaction(
    -- * Types
    AbsoluteValue()
    , Account(..)
    , AccountElement(..)
    , AccountTransaction(..)
    , AccountTypeable(..)
    , BalanceAmount(..)
    , BalanceSheet
    , Ledger(..)
    , PrintableString()
    , Transaction(..)
    , TransactionEntry(..)
    , TransactionType(..)
    -- * Functions
    , absoluteValue
    , accountTransaction
    , credit
    , debit
    , decrease
    , increase
    , ledgerTransaction
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

instance Accountable Account where
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

-- | Different transaction types
data TransactionType = Debit | Credit
    deriving (Eq, Enum, Bounded, Show)

-- | All different account elements (types)
data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show)

-- | A string that contains atleast one printable character
newtype PrintableString = PrintableString { unPrintableString :: Text.Text }
    deriving (Ord, Eq, Show)

data Account = Account {
    accountName :: PrintableString
    , accountElement :: AccountElement
} deriving (Ord, Eq, Show)

-- | Absolute value container for any absolute number, see constructor 'absoluteValue'
newtype AbsoluteValue a = AbsoluteValue { unAbsoluteValue :: a }
    deriving (Show)

-- | Constructor for 'AbsoluteValue'
-- This creates an absolute value buy applying 'Prelude.abs' to the argument
absoluteValue :: (Num a) => a -> AbsoluteValue a
absoluteValue = AbsoluteValue . abs

-- | Transaction entry is a single entry in a daybook or journal entry
data TransactionEntry a = TransactionEntry {
        teAccount :: Account -- ^ Account affected
        , teAmount :: TransactionAmount a -- ^ The amount to transfer
    } deriving (Show)

-- | Single atomic transaction of several entries
-- the list of entries should all be perfectly balanced
data Transaction a = Transaction {
    transactionDate :: Day -- ^ Date of the transaction
    , transactionEntries :: [TransactionEntry a] -- ^ Entries of transactions
} deriving (Show)

-- | The actual amount debited or credited
data TransactionAmount a = TransactionAmount TransactionType a
    deriving (Show, Eq)

-- | Account specific transaction that goes in to a ledger
data AccountTransaction a = AccountTransaction {
    atDate :: Day -- ^ Date of the transaction
    , atAmount :: TransactionAmount a -- ^ Amount debited or credited
} deriving (Show)

accountTransaction :: Day -> (a -> TransactionAmount a) -> a -> AccountTransaction a
accountTransaction date f = AccountTransaction date . f

ledgerTransaction :: Ledger a -> Day -> (Account -> a -> TransactionAmount a) -> a -> Ledger a
ledgerTransaction (Ledger account transactions) date f amount =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [accountTransaction date (f account) amount]

-- | Constructor for 'PrintableString'
printableString :: Text.Text -> Maybe PrintableString
printableString name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ PrintableString name

-- | Creates a credit transaction of amount a
credit :: a -> TransactionAmount a
credit = TransactionAmount Credit

-- | Creates a debit transaction of amount a
debit :: a -> TransactionAmount a
debit = TransactionAmount Debit

-- | Constructor for 'TransactionEntry'
transactionEntry :: 
    -- | Function to use to determine type of transaction
    (Account -> a -> TransactionAmount a)
     -> Account -- ^ Account to use for the transaction
     -> a -- ^ Amount to transfer
     -> TransactionEntry a -- ^ Return a new entry of a
transactionEntry f account = TransactionEntry account . f account

-- experimental below
type BalanceSheet a = AccountElement -> [Ledger a] -> BalanceAmount a
type ZeroBalance a = [TransactionAmount a] -> Bool

data BalanceAmount a = BalanceAmount TransactionType a
    deriving (Show)

data Ledger a = Ledger Account [AccountTransaction a]
    deriving(Show)

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
