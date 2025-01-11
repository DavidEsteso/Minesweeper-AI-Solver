{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_unordered_containers (
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
version = Version [0,2,20] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/bin"
libdir     = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/lib"
dynlibdir  = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/lib"
datadir    = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/share"
libexecdir = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/libexec"
sysconfdir = "/home/destcal/.cabal/store/ghc-8.8.4/unordered-containers-0.2.20-5b74b50cc4d0dc1bc269c43ef096123d37c1504930d7e6ba8c7c44242c7ccc2c/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unordered_containers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unordered_containers_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "unordered_containers_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "unordered_containers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unordered_containers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unordered_containers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
