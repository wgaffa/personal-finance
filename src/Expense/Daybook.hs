module Expense.Daybook
    ( Transaction(..)
    , TransactionEntry(..)
    ) where

import Data.Time (Day)

import Expense.Account (Account)
import Expense.Transaction

-- | Transaction entry is a single entry in a daybook or journal entry
data TransactionEntry a = TransactionEntry {
    account :: Account -- ^ Account affected
    , amount :: TransactionAmount a -- ^ The amount to transfer
} deriving (Show)

-- | Single atomic transaction of several entries
-- the list of entries should all be perfectly balanced
data Transaction a = Transaction {
    date :: Day -- ^ Date of the transaction
    , entries :: [TransactionEntry a] -- ^ Entries of transactions
} deriving (Show)
