{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_question (
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

bindir     = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/bin"
libdir     = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/lib/x86_64-linux-ghc-8.10.4/question-0.1.0.0-LowiW5pcct3hpcDm6EwzE-auto-grade"
dynlibdir  = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/share/x86_64-linux-ghc-8.10.4/question-0.1.0.0"
libexecdir = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/libexec/x86_64-linux-ghc-8.10.4/question-0.1.0.0"
sysconfdir = "/home/wolf/courses/312/cpsc-312-root-repo/pl-ubc-cpsc312/questions/quizzes/haskell/digs-abstracting/digit-to-count/tests/.stack-work/install/x86_64-linux-tinfo6/d6af7cfc132858dffd7fd721e590d1187422f1f53c93634f7fc929a753f8f41f/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "question_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "question_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "question_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "question_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "question_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "question_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
