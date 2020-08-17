module Expense.Transaction(
    -- * Types
    AbsoluteValue()
    , Account(..)
    , AccountElement(..)
    , AccountTransaction(..)
    , BalanceSheet
    , Ledger(..)
    , PrintableString()
    , Transaction(..)
    , TransactionAmount(..)
    , TransactionEntry(..)
    , TransactionType(..)
    -- * Functions
    , absoluteValue
    , credit
    , debit
    , decrease
    , increase
    , ledgerTransaction
    , printableString
    , splitTransactions
    , toNumeral
    , toSigNum
    , transactionEntry
    , transactionType
    , unAbsoluteValue
    , zeroBalance
) where

-- Text manipulation
import Data.Char
import qualified Data.Text as Text

-- time and date
import Data.Time (Day)

class Accountable f where
    transactionType :: f -> TransactionType
    increase :: f -> (a -> TransactionAmount a)
    decrease :: f -> (a -> TransactionAmount a)

instance Accountable Account where
    transactionType = transactionType . accountElement
    increase = increase . accountElement
    decrease = increase . accountElement

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

instance (Num a) => Semigroup (AbsoluteValue a) where
    (AbsoluteValue a) <> (AbsoluteValue b) = AbsoluteValue (a + b)

instance (Num a) => Monoid (AbsoluteValue a) where
    mempty = AbsoluteValue 0

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

instance Functor TransactionAmount where
    fmap f (TransactionAmount t amount) = TransactionAmount t $ f amount

-- | Account specific transaction that goes in to a ledger
data AccountTransaction a = AccountTransaction {
    atDate :: Day -- ^ Date of the transaction
    , atAmount :: TransactionAmount a -- ^ Amount debited or credited
} deriving (Show)

ledgerTransaction :: AccountTransaction a -> Ledger a -> Ledger a
ledgerTransaction transaction (Ledger account transactions) =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [transaction]

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
type BalanceSheet a b = AccountElement -> [Ledger a] -> TransactionAmount b
type ZeroBalance a = [TransactionAmount a] -> Bool

data Ledger a = Ledger Account [AccountTransaction a]
    deriving(Show)

zeroBalance :: (Eq a, Num a) => [TransactionAmount a]-> Bool
zeroBalance xs = foldr ((+) . toNumeral) 0 xs == 0

toNumeral :: (Num a) => TransactionAmount a -> a
toNumeral (TransactionAmount t x) = toSigNum t * x

toSigNum :: (Num a) => TransactionType -> a
toSigNum Debit = 1
toSigNum Credit = -1

-- | Split transactions in debits and credits
splitTransactions :: [TransactionAmount a] -> ([TransactionAmount a], [TransactionAmount a])
splitTransactions =
    foldr (\ trans@(TransactionAmount t _) res -> splitTypes t trans res) ([], [])
  where
    splitTypes Debit x (debits, credits) = (x:debits, credits)
    splitTypes Credit x (debits, credits) = (debits, x:credits)