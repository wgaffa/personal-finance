{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.PrettyPrint
    ( printAccount
    , printJournal
    , printListAccounts
    , renderLedger
    , renderTriageBalance
    , renderCompleteLedger
    , formatColumns
    , transactionAmountRow
    , numberField
    ) where

import Prelude hiding ((<>))

import Data.UUID
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

printJournal :: (Integral a, Show a) =>
    Day -> [(Account, AccountTransaction a)] -> IO ()
printJournal date = printBox . renderJournal date

renderJournal :: (Integral a, Show a) =>
    Day -> [(Account, AccountTransaction a)] -> Box
renderJournal date xs =
    title // separator // body // separator // totals
  where
    body = hsep 2 top
        . map formatColumns
        . transpose
        . (++) [headers]
        . map (uncurry journalRow)
        $ xs
    title = alignHoriz center2 width . text $ "Transaction for " ++ show date
    width = cols body
    separator = text $ replicate width '-'
    headers = ["Account", "Debit", "Credit", "Description"]
    totals = hsep 2 left $ text "Total" : map (text . Text.unpack) balances
    balances =
        let toAmount (TransactionAmount _ a) = a
            (debits, credits) = mapPair (map toAmount) $ splitTransactions transactions
            in [numberField . theSum $ debits
                , numberField . theSum $ credits]
    theSum = getSum . mconcat . map Sum
    mapPair f = uncurry ((,) `on` f)
    transactions = map (amount . snd) xs

journalRow ::
    (Integral a) =>
    Account -> AccountTransaction a -> [Text.Text]
journalRow Account{..} AccountTransaction{..} =
        (unAccountName name:transactionAmountRow amount) ++
            [maybe Text.empty Text.pack description]

renderCompleteLedger :: (Integral a) => Ledger a -> String
renderCompleteLedger = renderLedger transactionRow headers
  where
    headers = ["Date", "Debit", "Credit", "Description", "Id"]

renderLedger :: 
  (Integral a) 
    => ([AccountTransaction a -> Text.Text])
    -> [Text.Text]
    -> Ledger a
    -> String
renderLedger row headers ledger@(Ledger account transactions) =
    render $ title // separator // body // separator // balance
  where
    body =
        hsep 2 top
        . map formatColumns
        . transpose
        . (++) [headers]
        . map (sequenceA row) $ transactions
    title = alignHoriz center2 width (boxAccount account)
    separator = text $ replicate width '-'
    balance = text "Balance:" <+> (
        hsep 1 left
        . map (text . Text.unpack)
        . balanceRow
        . accountBalance $ ledger)
    width = cols body

transactionRow :: (Integral a) => ([AccountTransaction a -> Text.Text])
transactionRow =
      [ Text.pack . show . date
      , debitColumn
      , creditColumn
      , maybe Text.empty Text.pack . description
      , toText . transactionId
      ]
  where
    debitColumn AccountTransaction{..} = toAmount Debit amount
    creditColumn AccountTransaction{..} = toAmount Credit amount
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
    , Text.pack $ printf "%.2f" (fromIntegral a / 100 :: Double)]

accountRow :: Account -> [Text.Text]
accountRow Account{..} =
    [ Text.pack . show $ unAccountNumber number
    , unAccountName name
    , Text.pack $ show element]

numberField :: (Integral a ) => a -> Text.Text
numberField x = Text.pack $ printf "%.2f" (fromIntegral x / 100 :: Double)
