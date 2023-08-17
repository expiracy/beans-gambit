{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cswk_gambit_two (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/bin"
libdir     = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/lib/x86_64-linux-ghc-9.2.5/cswk-gambit-two-0.1.0.0-6vSUyAcYzxHEjD09DBoOLZ-gambit"
dynlibdir  = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/share/x86_64-linux-ghc-9.2.5/cswk-gambit-two-0.1.0.0"
libexecdir = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/libexec/x86_64-linux-ghc-9.2.5/cswk-gambit-two-0.1.0.0"
sysconfdir = "/dcs/22/u2227951/Downloads/Test/cswk-gambit-two/.stack-work/install/x86_64-linux/e0fa15a0e8f88fca03674163ac88082c43d9a92eef44b638a3037bcd06eaf6a5/9.2.5/etc"

getBinDir     = catchIO (getEnv "cswk_gambit_two_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cswk_gambit_two_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cswk_gambit_two_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cswk_gambit_two_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cswk_gambit_two_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cswk_gambit_two_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
