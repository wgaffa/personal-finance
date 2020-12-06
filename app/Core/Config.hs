module Core.Config (
    getConfigFile,
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

import Control.Monad.IO.Class (MonadIO, liftIO)

import Core.App (appIdentifier)

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
