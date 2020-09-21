{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Expense.Account
    ( Ledger(..)
    , Account(..)
    , AccountElement(..)
    , AccountName()
    , AccountNumber()
    , AccountTransaction(..)
    , Accountable
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

data Ledger a = Ledger Account [AccountTransaction a]
    deriving(Show)

instance Functor Ledger where
    fmap f x@(Ledger a ts) = Ledger a $ map (fmap f) ts

-- | All different account elements (types)
data AccountElement = Asset | Liability | Equity | Income | Expenses
    deriving (Ord, Eq, Enum, Bounded, Show, Read)

-- | A string that contains atleast one printable character
newtype AccountName = AccountName Text.Text
    deriving (Ord, Eq)
    deriving newtype (Show)

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

-- | Account specific transaction that goes in to a ledger
data AccountTransaction a = AccountTransaction {
    date :: Day -- ^ Date of the transaction
    , description :: Maybe String -- ^ Description for the transaction
    , amount :: TransactionAmount a -- ^ Amount debited or credited
} deriving (Show)

instance Functor AccountTransaction where
    fmap f t = t{amount = fmap f (amount t)}

ledgerTransaction :: AccountTransaction a -> Ledger a -> Ledger a
ledgerTransaction transaction (Ledger account transactions) =
    Ledger account appendTransaction
  where
    appendTransaction = transactions ++ [transaction]

accountBalance :: (Num a) => Ledger a -> TransactionAmount a
accountBalance (Ledger acc ts) =
    toBalance acc id
    . balance $ ts
  where
    balance = foldr ((+) . toNumeral . amount) 0
