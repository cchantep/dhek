module Dhek.File where

import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (traverse_)

import Dhek.Instr
import Dhek.Types
import Dhek.Utils (takeExtension)

onJsonSave :: DhekProgram ()
onJsonSave = compile $ do
    fOpt <- selectJsonFile
    traverse_ go fOpt
  where
    go path = do
        nb <- getPageCount
        rs <- getAllRects
        let save = saveNew $ fillUp nb rs
            path1
                | takeExtension path == ".json" = path
                | otherwise                     = path ++ ".json"
        performIO $
            B.writeFile path1 (encode save)
