module OptParser where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { dbConnection :: String }

options :: Parser Options
options =
    Options
    <$> argument str (metavar "FILE")