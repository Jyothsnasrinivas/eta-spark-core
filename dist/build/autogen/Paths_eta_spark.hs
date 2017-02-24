module Paths_eta_spark (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jyothsnasrinivas/.epm/bin"
libdir     = "/Users/jyothsnasrinivas/.epm/lib/x86_64-osx-eta-0.0.5-ghc7_10_3/eta-spark-0.1.0.0"
datadir    = "/Users/jyothsnasrinivas/.epm/share/x86_64-osx-eta-0.0.5-ghc7_10_3/eta-spark-0.1.0.0"
libexecdir = "/Users/jyothsnasrinivas/.epm/libexec"
sysconfdir = "/Users/jyothsnasrinivas/.epm/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eta_spark_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eta_spark_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "eta_spark_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eta_spark_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eta_spark_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
