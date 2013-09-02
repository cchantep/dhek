{-# LANGUAGE CPP #-}
module Utils where

#include "config.h"

#if defined __WINDOWS__
import System.FilePath.Windows (splitDirectories)
#endif

sanitizeFilePath :: FilePath -> FilePath
#if defined __WINDOWS__
sanitizeFilePath path = let xs = splitDirectories path in intersperse '/' xs
#else
sanitizeFilePath path = path
#endif
