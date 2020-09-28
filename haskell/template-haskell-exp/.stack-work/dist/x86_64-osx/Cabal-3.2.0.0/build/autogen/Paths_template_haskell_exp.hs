{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_template_haskell_exp (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/bin"
libdir     = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/lib/x86_64-osx-ghc-8.10.2/template-haskell-exp-0.1.0.0-8389dwOTvccmcCYoyuxGT"
dynlibdir  = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/lib/x86_64-osx-ghc-8.10.2"
datadir    = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/share/x86_64-osx-ghc-8.10.2/template-haskell-exp-0.1.0.0"
libexecdir = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/libexec/x86_64-osx-ghc-8.10.2/template-haskell-exp-0.1.0.0"
sysconfdir = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/2a2601e9a16bad28f7400bc6ce46ed0bc103611c49a9d4289da1d03aeb9b6854/8.10.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "template_haskell_exp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "template_haskell_exp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "template_haskell_exp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "template_haskell_exp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "template_haskell_exp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "template_haskell_exp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
