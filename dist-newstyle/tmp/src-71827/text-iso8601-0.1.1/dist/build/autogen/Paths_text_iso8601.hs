{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_text_iso8601 (
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
version = Version [0,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/bin"
libdir     = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/lib"
dynlibdir  = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/lib"
datadir    = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/share"
libexecdir = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/libexec"
sysconfdir = "/home/destcal/.cabal/store/ghc-8.8.4/text-iso8601-0.1.1-ce4fa1cfd1c7b162a0f0c97bb98db997a6935b04d3e73ea21d19c61885793d3f/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "text_iso8601_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "text_iso8601_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "text_iso8601_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "text_iso8601_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_iso8601_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_iso8601_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
