module Expense.Transaction(
    Transaction()
    , credit
    , debit
    , transaction
) where

data Transaction a = Debit a | Credit a
    deriving (Show, Eq)

instance (Ord a, Num a) => Semigroup (Transaction a) where
    (<>) = mappend

instance (Ord a, Num a) => Monoid (Transaction a) where
    mempty = Debit 0
    mappend a b = transaction (toNum a + toNum b)

toNum :: (Num a) => Transaction a -> a
toNum (Credit x) = negate x
toNum (Debit x) = x

transaction :: (Ord a, Num a) => a -> Transaction a
transaction amount
    | amount < 0 = credit amount
    | amount > 0 = debit amount
    | otherwise = Debit 0

credit :: (Num a) => a -> Transaction a
credit = Credit . abs

debit :: (Num a) => a -> Transaction a
debit = Debit . abs