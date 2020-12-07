{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Config (
    getConfigFile,
    buildConfig,
) where

import System.Directory (
    XdgDirectory (XdgConfig),
    doesFileExist,
    getCurrentDirectory,
    getXdgDirectory,
 )
import System.FilePath ((</>))

import Data.Functor ((<&>))
import Data.List
import Data.Monoid

import GHC.Generics (Generic)

import Generics.Deriving.Monoid

import Control.Monad.IO.Class (MonadIO, liftIO)

import Core.App (appIdentifier)
import Data.Maybe (fromMaybe)

newtype Config select = Config
    {database :: select Last FilePath}
    deriving (Generic)

newtype Build f a = Build {fromBuild :: f a}
    deriving (Eq, Foldable, Functor, Semigroup, Monoid, Ord, Show, Traversable)

newtype Run f a = Run {fromRun :: a}
    deriving (Eq, Foldable, Functor, Semigroup, Monoid, Ord, Show, Traversable)

instance Semigroup (Config Build) where
    (<>) = mappend

instance Monoid (Config Build) where
    mempty = memptydefault
    mappend = mappenddefault

-- | Name of the applicaitons configuration file to use
configFileName :: FilePath
configFileName = "config.cfg"

{- | The config dir to use for config files under a path
 Should be the applications name/identifier
-}
configDir :: FilePath
configDir = appIdentifier

-- | Retrieve the path to the config directory
homeConfigDir :: (MonadIO m) => m FilePath
homeConfigDir = liftIO $ getXdgDirectory XdgConfig configDir

-- | Retrieve the path to the home config file
homeConfig :: (MonadIO m, MonadFail m) => m FilePath
homeConfig = homeConfigDir <&> (</> configFileName)

-- | The files to check for in reverse order
checkConfigFiles :: IO [FilePath]
checkConfigFiles = sequenceA [homeConfig, fmap (</> configFileName) getCurrentDirectory]

getConfigFile :: (MonadIO m, MonadFail m) => m FilePath
getConfigFile = do
    cfgFiles <- liftIO checkConfigFiles
    res <- liftIO $ mapM doesFileExist cfgFiles
    let cfgFile = find snd (zip cfgFiles res) <&> fst
    case cfgFile of
        Nothing -> fail "no config file found"
        Just x -> return x

infix 1 !<-
(!<-) :: (f a -> b) -> Build f a -> Run f b
(!<-) f = Run . f . fromBuild

buildConfig :: Config Build -> Config Run
buildConfig Config {..} =
    Config
        { database = fromMaybe "db.sqlite3" . getLast !<- database
        }
