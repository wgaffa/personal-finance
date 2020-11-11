module Expense.Transaction(
    -- * Types
    TransactionAmount(..)
    , TransactionType(..)
    -- * Functions
    -- ** Constructors
    , credit
    , debit
    -- ** Transformations
    , splitTransactions
    , toNumeral
    -- ** Calculations
    , zeroBalance
    , mzeroBalance
    , balance
    -- ** Parsing
    , parseTransactionAmount
) where

import Data.Function ( on )

-- | Different transaction types
data TransactionType = Debit | Credit
    deriving (Eq, Enum, Bounded, Show, Read)

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

-- | Calculate wether the balance on the transactions are equal
zeroBalance :: (Eq a, Num a) => [TransactionAmount a] -> Bool
zeroBalance xs = balance xs == 0

-- | Calculate the numeral balance for a list of transactions
balance :: (Num a) => [TransactionAmount a] -> a
balance = foldr ((+) . toNumeral) 0

-- | Overloaded version for monoids of `zeroBalance`
mzeroBalance :: (Eq a, Monoid a) => [TransactionAmount a] -> Bool
mzeroBalance xs =
    let toAmount (TransactionAmount _ a) = a
        (debits, credits) = mapPair (map toAmount) $ splitTransactions xs
    in mconcat debits == mconcat credits
  where
    mapPair f = uncurry ((,) `on` f)

{-|
    Return the numeral representation of a transaction entry.

    A Debit is represented as positive and Credit as negative
-}
toNumeral :: (Num a) => TransactionAmount a -> a
toNumeral (TransactionAmount t x) = x * case t of
    Debit -> 1
    Credit -> -1

-- | Split transactions in debits and credits
splitTransactions ::
    [TransactionAmount a]
    -> ([TransactionAmount a], [TransactionAmount a])
splitTransactions =
    foldr split ([], [])
  where
    split trans@(TransactionAmount t _) acc = splitTypes t trans acc
    splitTypes Debit x (debits, credits) = (x:debits, credits)
    splitTypes Credit x (debits, credits) = (debits, x:credits)

parseTransactionAmount :: (Ord a, Num a) => (a -> b) -> a -> TransactionAmount b
parseTransactionAmount f n
    | n < 0 = credit (f n)
    | otherwise = debit (f n)
