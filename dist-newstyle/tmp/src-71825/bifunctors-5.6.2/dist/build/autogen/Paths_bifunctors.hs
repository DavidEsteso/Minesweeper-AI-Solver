{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bifunctors (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [5,6,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/bin"
libdir     = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/lib"
dynlibdir  = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/lib"
datadir    = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/share"
libexecdir = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/libexec"
sysconfdir = "/home/destcal/.cabal/store/ghc-8.8.4/bifunctors-5.6.2-6fb57d8c3bff3aec54551ad39bda082d00b790d529e8f1eeffb184d4117b64e1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bifunctors_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bifunctors_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bifunctors_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bifunctors_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bifunctors_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bifunctors_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
