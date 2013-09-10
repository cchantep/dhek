{-# LANGUAGE CPP #-}
module Utils (takeFileName, takeExtension)  where

#include "config.h"

#if defined __WINDOWS__
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
