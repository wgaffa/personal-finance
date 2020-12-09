{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Core.App (
    App (..),
    Command (..),
    ShowOptions (..),
    appIdentifier,
) where

import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    MonadThrow,
 )
import Control.Monad.Except (
    ExceptT,
    MonadError,
 )
import Control.Monad.IO.Class (MonadIO ())
import Control.Monad.Reader (
    MonadReader (),
    ReaderT,
 )

import Core.Config (Config, Run)
import Core.Error (AccountError)

{- | Different commands that can be passed to the application and
 their arguments if used
-}
data Command
    = List
    | CreateAccount
    | AddTransaction
    | ShowAccount ShowOptions
    | UpdateDatabase
    | CheckHealth
    | AccountingPeriod
    | NewAccountingPeriod String

-- | Type that holds the arguments of the command /show/
data ShowOptions = ShowOptions
    { -- |The account number to show
      filterAccount :: Int
    , showId :: Bool
    }

newtype App a = App
    { runApp :: ReaderT (Config Run) (ExceptT AccountError IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving newtype (MonadReader (Config Run), MonadError AccountError)
    deriving newtype (MonadThrow, MonadCatch, MonadMask, MonadFail)

appIdentifier :: String
appIdentifier = "dee-book"
