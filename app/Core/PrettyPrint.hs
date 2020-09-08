{-# LANGUAGE RecordWildCards #-}

module Core.PrettyPrint
    ( printAccount
    , printListAccounts
    , printTransactions
    , formatColumns
    ) where

import qualified Data.Text as Text

import Data.List (transpose)

import Text.PrettyPrint.Boxes

import Expense.Transaction
import Expense.Account

formatColumns :: [Text.Text] -> Box
formatColumns items =
    vcat left
    . map (text . Text.unpack . Text.justifyLeft width ' ') $ items
  where
    width = (+2) . maximum $ map Text.length items

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
    . hcat top
    . map formatColumns
    . transpose
    . map accountRow

printTransactions :: (Integral a) => [AccountTransaction a] -> IO ()
printTransactions =
    printBox
    . hcat top
    . map formatColumns
    . transpose
    . map transactionRow

transactionRow :: (Integral a) => AccountTransaction a -> [Text.Text]
transactionRow AccountTransaction{..} =
    [Text.pack $ show date]
        ++ transactionAmountRow amount
        ++ [maybe Text.empty Text.pack description]

transactionAmountRow :: (Integral a) => TransactionAmount a -> [Text.Text]
transactionAmountRow (TransactionAmount t a) =
    [ Text.pack $ show t
    , Text.pack . show . (/100) . fromIntegral $ a]

accountRow :: Account -> [Text.Text]
accountRow Account{..} =
    [ Text.pack . show $ unAccountNumber number
    , unAccountName name
    , Text.pack $ show element]
