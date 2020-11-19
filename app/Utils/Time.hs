module Utils.Time (
    today,
) where

import Data.Time (Day, getCurrentTime, utctDay)

today :: IO Day
today = utctDay <$> getCurrentTime
