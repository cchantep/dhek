{-# LANGUAGE CPP #-}
module Utils where

#include "config.h"

#if defined __WINDOWS__
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
