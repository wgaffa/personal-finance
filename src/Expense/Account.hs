{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Expense.Account
    ( Ledger(..)
    , LedgerEntry(..)
    , Account(..)
    , AccountElement(..)
    , AccountName()
    , AccountNumber()
    , Accountable
    , Details(..)
    , Journal(..)
    , JournalEntry(..)
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
    , toBalance
    ) where

import Data.Char ( isSpace )
import qualified Data.Text as Text
import Data.Time (Day)

import Expense.Transaction
    ( TransactionAmount(..),
      TransactionType(..),
      credit,
      debit,
      toNumeral )

data Details = Details {
    date :: Day,
    description :: Maybe String
    }

-- | A journal is a daily entry and has all entries for a complete
-- transaction. All entries should be balanced (debit and credit totals are equal)
data Journal a = Journal Details [JournalEntry a]
data JournalEntry a = JournalEntry Account (TransactionAmount a)

instance Functor Journal where
    fmap f (Journal d xs) = Journal d . map (fmap f) $ xs

instance Functor JournalEntry where
    fmap f (JournalEntry a x) = JournalEntry a . fmap f $ x

instance Functor Ledger where
    fmap f (Ledger d xs) = Ledger d . map (fmap f) $ xs

instance Functor LedgerEntry where
    fmap f (LedgerEntry d x) = LedgerEntry d . fmap f $ x

-- | A ledger holds an accounts transactions
data Ledger a = Ledger Account [LedgerEntry a]
data LedgerEntry a = LedgerEntry Details (TransactionAmount a)

-- | All different account elements (types)
data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show, Read)

-- | A string that contains atleast one printable character
newtype AccountName = AccountName Text.Text
    deriving (Ord, Eq)
    deriving newtype (Show)

-- | Unwrap the name from an `AccountName`
unAccountName :: AccountName -> Text.Text
unAccountName (AccountName n) = n

-- | Smart constructor for 'AccountName'
accountName :: Text.Text -> Maybe AccountName
accountName name
    | Text.all isSpace name = Nothing
    | otherwise = Just $ AccountName name

-- | An account number is any positive number
newtype AccountNumber = AccountNumber Int
    deriving (Ord, Eq)
    deriving newtype (Show)

unAccountNumber :: AccountNumber -> Int
unAccountNumber (AccountNumber x) = x

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
    -- | Fixes the sign for a balance amount, when the transaction
    -- | types does not match we negate the number given by (a -> b)
    toBalance :: (Num b) =>
        f -> (a -> b) -> a -> TransactionAmount b
    toBalance x f a
        | transactionType x == Debit = TransactionAmount (transactionType x) (f a)
        | otherwise = TransactionAmount (transactionType x) . negate $ f a

instance Accountable Account where
    transactionType = transactionType . element
    increase = increase . element
    decrease = decrease . element

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

ledgerTransaction :: LedgerEntry a -> Ledger a -> Ledger a
ledgerTransaction transaction (Ledger account transactions) =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [transaction]

accountBalance :: (Num a) => Ledger a -> TransactionAmount a
accountBalance (Ledger acc ts) =
    toBalance acc id
    . balance $ ts
  where
    balance = foldr ((+) . toNumeral . getAmount) 0
    getAmount (LedgerEntry _ x) = x
