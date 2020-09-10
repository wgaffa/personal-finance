{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.PrettyPrint
    ( printAccount
    , printListAccounts
    , printLedger
    , formatColumns
    ) where

import Prelude hiding ((<>))
import qualified Data.Text as Text

import Data.List (transpose)

import Text.Printf
import Text.PrettyPrint.Boxes

import Expense.Transaction
import Expense.Account

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

printLedger :: (Integral a) => Ledger a -> IO ()
printLedger = printBox . renderLedger

renderLedger :: (Integral a) => Ledger a -> Box
renderLedger ledger@(Ledger account transactions) =
    title // separator // body // separator // balance
  where
    body =
        hsep 2 top
        . map formatColumns
        . transpose
        . (++) [headers]
        . map transactionRow $ transactions
    title = alignHoriz center2 width (boxAccount account)
    separator = text $ replicate width '-'
    headers = ["Date", "Debit/Credit", "Amount", "Description"]
    balance = text "Balance:" <+> (
        hsep 1 left
        . map (text . Text.unpack)
        . transactionAmountRow
        . accountBalance $ ledger)
    width = cols body

transactionRow :: (Integral a) => AccountTransaction a -> [Text.Text]
transactionRow AccountTransaction{..} =
    [Text.pack $ show date]
        ++ transactionAmountRow amount
        ++ [maybe Text.empty Text.pack description]

transactionAmountRow :: (Integral a) => TransactionAmount a -> [Text.Text]
transactionAmountRow (TransactionAmount t a) =
    [ Text.pack $ show t
    , Text.pack $ printf "%.2f" (fromIntegral a / 100 :: Double)]

accountRow :: Account -> [Text.Text]
accountRow Account{..} =
    [ Text.pack . show $ unAccountNumber number
    , unAccountName name
    , Text.pack $ show element]
