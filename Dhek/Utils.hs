{-# LANGUAGE CPP #-}
module Dhek.Utils (joinPath, takeFileName, takeExtension, trimString)  where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

#include "../config.h"

#if defined __WINDOWS__
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif

trimString :: String -> String
trimString = dropWhileEnd isSpace . dropWhile isSpace
