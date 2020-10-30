{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.PrettyPrint
    ( printAccount
    , renderJournal
    , printListAccounts
    , renderLedger
    , renderTriageBalance
    , formatColumns
    , transactionAmountRow
    , numberField
    ) where

import Prelude hiding ((<>))

import Data.Maybe (fromMaybe)
import Data.Time (Day)
import qualified Data.Text as Text

import Data.Monoid (Sum(Sum), getSum)
import Data.Function (on)
import Data.List (transpose)

import Text.Printf ( printf )
import Text.PrettyPrint.Boxes
    ((//),
      (<+>),
      alignHoriz,
      center2,
      hsep,
      left,
      printBox,
      render,
      text,
      top,
      vcat,
      vsep,
      Box(cols) )

import Expense.Transaction
import Expense.Account
import Utility.Absolute

formatColumns :: [Text.Text] -> Box
formatColumns items =
    vcat left
    . map (text . Text.unpack . Text.justifyLeft width ' ') $ items
  where
    width = maximum $ map Text.length items

boxAccount :: Account -> Box
boxAccount (Account number name element) =
    hsep 2 left boxes
  where
    boxes = [
        text (show . unAccountNumber $ number)
        , text (Text.unpack . unAccountName $ name)
        , text (show element)
       ]

printAccount :: Account -> IO ()
printAccount = printBox . boxAccount

printListAccounts :: [Account] -> IO ()
printListAccounts =
    printBox
    . hsep 2 top
    . map formatColumns
    . transpose
    . map accountRow

renderTriageBalance ::
    (Integral a, Show a)
    => [(Account, a)] -> String
renderTriageBalance xs =
    render table
  where
    table = body
    headers = ["Id", "Name", "Element", "Balance"]
    body = hsep 2 top
      . map formatColumns
      . transpose
      . (++) [headers]
      . map (sequenceA row) $ xs
    row =
        [ Text.pack . show . number . fst,
          unAccountName . name . fst,
          Text.pack . show . element . fst,
          numberField . snd]

renderJournal:: (Integral a, Show a) => Journal a -> String
renderJournal (Journal details txs) =
    render $ title // separator // body // separator // totals
  where
    body = hsep 2 top
        . map formatColumns
        . transpose
        . (++) [headers]
        . map journalEntryRow
        $ txs
    title = alignHoriz center2 width . text $ titleText
    titleText =
        "Transaction for " ++ show (date details) ++ " - "
          ++ fromMaybe mempty (description details)
    width = cols body
    separator = text $ replicate width '-'
    headers = ["Account", "Debit", "Credit"]
    totals = hsep 2 left $ text "Total" : map (text . Text.unpack) balances
    balances =
        let toAmount (TransactionAmount _ a) = a
            (debits, credits) = mapPair (map toAmount) $ splitTransactions transactions
            in [numberField . theSum $ debits
                , numberField . theSum $ credits]
    theSum = getSum . mconcat . map Sum
    mapPair f = uncurry ((,) `on` f)
    transactions = map amount txs
    amount (JournalEntry _ x) = x

-- | Make a row consisting of the columns AccountName, Debit, Credit
journalEntryRow ::
    (Integral a) => JournalEntry a -> [Text.Text]
journalEntryRow (JournalEntry account amount)=
        (getName account:transactionAmountRow amount)
  where
    getName = unAccountName . name

renderLedger ::
  (Integral a) => Ledger a -> String
renderLedger ledger@(Ledger account entries) =
    render $ title // separator // body // separator // balance
  where
    body =
        hsep 2 top
        . map formatColumns
        . transpose
        . (++) [headers]
        . map ledgerRow $ entries
    title = alignHoriz center2 width (boxAccount account)
    separator = text $ replicate width '-'
    balance = text "Balance:" <+> (
        hsep 1 left
        . map (text . Text.unpack)
        . balanceRow
        . accountBalance $ ledger)
    width = cols body
    headers = ["Date", "Debit", "Credit", "Description"]

ledgerRow :: (Integral a) => LedgerEntry a -> [Text.Text]
ledgerRow (LedgerEntry details amount)=
      [ Text.pack . show . date $ details
      , amountField Debit
      , amountField Credit
      , maybe Text.empty Text.pack . description $ details
      ]
  where
    amountField = flip toAmount amount
    toAmount expected (TransactionAmount t a)
      | expected == t = numberField a
      | otherwise = Text.empty

transactionAmountRow :: (Integral a) => TransactionAmount a -> [Text.Text]
transactionAmountRow (TransactionAmount Debit a) =
    [numberField a, Text.empty]
transactionAmountRow (TransactionAmount Credit a) =
    [Text.empty, numberField a]

balanceRow :: (Integral a) => TransactionAmount a -> [Text.Text]
balanceRow (TransactionAmount t a) =
    [ Text.pack $ show t
    , numberField a]

accountRow :: Account -> [Text.Text]
accountRow Account{..} =
    [ Text.pack . show $ unAccountNumber number
    , unAccountName name
    , Text.pack $ show element]

numberField :: (Integral a ) => a -> Text.Text
numberField x = Text.pack $ printf "%.2f" (fromIntegral x / 100 :: Double)
