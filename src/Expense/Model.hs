module Expense.Model (
    Recipient()
    , Tag()
    , recipient
    , tag
    , unRecipient
    , unTag
    , module Expense.Transaction
) where

import Data.Char
import qualified Data.Text as Text

import Data.Set (Set)

import Data.Time (Day)

-- Local imports
import Expense.Transaction

newtype Tag = Tag { unTag :: Text.Text }
    deriving (Show)
newtype Recipient = Recipient { unRecipient :: Text.Text }
    deriving (Show)

recipient :: Text.Text -> Maybe Recipient
recipient recip
    | Text.all isSpace recip = Nothing
    | otherwise = Just $ Recipient recip

tag :: Text.Text -> Maybe Tag
tag t
    | Text.all isSpace t = Nothing
    | otherwise = Just $ Tag $ Text.toLower t
