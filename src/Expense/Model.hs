module Expense.Model (
    Recipient()
    , Tag()
    , Transaction()
    , Expense(..)
    , credit
    , debit
    , recipient
    , tag
    , transaction
    , unRecipient
    , unTag
) where

import Data.Char
import qualified Data.Text as Text

import Data.Set (Set)

import Data.Time (Day)

newtype Tag = Tag { unTag :: Text.Text }
    deriving (Show)
newtype Recipient = Recipient { unRecipient :: Text.Text }
    deriving (Show)

data Transaction a = Debit a | Credit a
    deriving (Show)

data Expense = Expense {
    expenseAmount :: Transaction Integer
    , expenseTags :: Set Tag
    , expenseDescription :: Text.Text
    , expenseRecipient :: Recipient
    , expenseDate :: Day
} deriving (Show)

recipient :: Text.Text -> Maybe Recipient
recipient recip
    | Text.all isSpace recip = Nothing
    | otherwise = Just $ Recipient recip

tag :: Text.Text -> Maybe Tag
tag t
    | Text.all isSpace t = Nothing
    | otherwise = Just $ Tag $ Text.toLower t

transaction :: (Ord a, Num a) => a -> Maybe (Transaction a)
transaction amount
    | amount < 0 = Just $ credit amount
    | amount > 0 = Just $ debit amount
    | otherwise = Nothing

credit :: (Num a) => a -> Transaction a
credit = Credit . abs

debit :: (Num a) => a -> Transaction a
debit = Debit . abs