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

bindir     = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/bin"
libdir     = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/lib/x86_64-osx-ghc-8.8.4/template-haskell-exp-0.1.0.0-K4rkvp8X3l4HwD3WybYvMb"
dynlibdir  = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/share/x86_64-osx-ghc-8.8.4/template-haskell-exp-0.1.0.0"
libexecdir = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/libexec/x86_64-osx-ghc-8.8.4/template-haskell-exp-0.1.0.0"
sysconfdir = "/Users/shayne/project/template-haskell-exp/.stack-work/install/x86_64-osx/795c09543aa857801c8fce1dd95a8916fb144f193736c2a2ca8938fd0b362352/8.8.4/etc"

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
