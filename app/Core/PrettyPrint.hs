{-# LANGUAGE RecordWildCards #-}

module Core.PrettyPrint
    ( printAccount
    , printListAccounts
    , formatColumns
    ) where

import Prelude hiding ((<>))
import qualified Data.Text as Text

import Data.List (transpose)

import Text.PrettyPrint.Boxes

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

accountRow :: Account -> [Text.Text]
accountRow Account{..} =
        [ Text.pack . show $ unAccountNumber number
        , unAccountName name
        , Text.pack $ show element]
