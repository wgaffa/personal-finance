module Core.PrettyPrint
    ( printAccount
    ) where

import qualified Data.Text as Text

import Expense.Account

printAccount :: Account -> IO ()
printAccount (Account number name element) = do
    putStr $ (Text.unpack . unAccountName $ name) ++ " (" ++ (show . unAccountNumber $ number) ++ ") " ++ show element
