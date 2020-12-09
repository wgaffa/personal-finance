{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Config (
    Config (..),
    Build (..),
    Run (..),
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
import Data.Maybe (fromMaybe)
import Data.Monoid

import GHC.Generics (Generic)

import Generics.Deriving.Monoid

import Control.Monad.IO.Class (MonadIO, liftIO)

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

-- | Retrieve the path to the config directory
homeConfigDir :: (MonadIO m) => FilePath -> m FilePath
homeConfigDir configDir = liftIO $ getXdgDirectory XdgConfig configDir

-- | The files to check for in reverse order
checkConfigFiles :: FilePath -> FilePath -> IO [FilePath]
checkConfigFiles baseDir configFile = sequenceA [(</> configFile) <$> homeConfigDir baseDir, fmap (</> configFile) getCurrentDirectory]

getConfigFile :: (MonadIO m, MonadFail m) => FilePath -> FilePath -> m FilePath
getConfigFile baseDir configFile = do
    cfgFiles <- liftIO $ checkConfigFiles baseDir configFile
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
