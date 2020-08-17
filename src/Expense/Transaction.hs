module Expense.Transaction(
    -- * Types
    TransactionAmount(..)
    , TransactionType(..)
    -- * Functions
    , credit
    , debit
    , splitTransactions
    , toNumeral
    , toSigNum
    , zeroBalance
    , mzeroBalance
) where

-- Text manipulation
import Data.Char
import qualified Data.Text as Text

import Data.Function

-- time and date
import Data.Time (Day)

-- | Different transaction types
data TransactionType = Debit | Credit
    deriving (Eq, Enum, Bounded, Show)

-- | The actual amount debited or credited
data TransactionAmount a = TransactionAmount TransactionType a
    deriving (Show, Eq)

instance Functor TransactionAmount where
    fmap f (TransactionAmount t amount) = TransactionAmount t $ f amount

-- | Creates a credit transaction of amount a
credit :: a -> TransactionAmount a
credit = TransactionAmount Credit

-- | Creates a debit transaction of amount a
debit :: a -> TransactionAmount a
debit = TransactionAmount Debit

-- experimental below
-- type BalanceSheet a b = AccountElement -> [Ledger a] -> TransactionAmount b
type ZeroBalance a = [TransactionAmount a] -> Bool

zeroBalance :: (Eq a, Num a) => [TransactionAmount a]-> Bool
zeroBalance xs = foldr ((+) . toNumeral) 0 xs == 0

mzeroBalance :: (Eq a, Monoid a) => [TransactionAmount a] -> Bool
mzeroBalance xs =
    let toAmount (TransactionAmount _ a) = a
        (debits, credits) = mapPair (map toAmount) $ splitTransactions xs
    in mconcat debits == mconcat credits

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = uncurry ((,) `on` f)

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