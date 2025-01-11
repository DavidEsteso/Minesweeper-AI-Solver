{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_attoparsec (
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
version = Version [0,14,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/bin"
libdir     = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/lib"
dynlibdir  = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/lib"
datadir    = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/share"
libexecdir = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/libexec"
sysconfdir = "/home/destcal/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-26e5b906efd307923e9888cd78aaf76b1385080bec1153e5e6fa70449c1beaf3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "attoparsec_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "attoparsec_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "attoparsec_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "attoparsec_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "attoparsec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "attoparsec_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
