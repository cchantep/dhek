{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.File
--
-- File menu items handlers
--------------------------------------------------------------------------------
module Dhek.File where

--------------------------------------------------------------------------------
import Control.Exception
import Data.Foldable (traverse_)

--------------------------------------------------------------------------------
import           Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           System.FilePath (takeExtension)

--------------------------------------------------------------------------------
import Dhek.Free
import Dhek.Instr
import Dhek.Types

--------------------------------------------------------------------------------
onJsonSave :: DhekProgram ()
onJsonSave = compile $ do
    fOpt <- selectJsonFile
    traverse_ go fOpt
  where
    go path = do
        nb <- getPageCount
        rs <- getAllRects
        let nb   = length rs
            save = saveNew $ fillUp nb rs
            path1
                | takeExtension path == ".json" = path
                | otherwise                     = path ++ ".json"

        e <- performIO $ try $ B.writeFile path1 (encode save)
        either exception return e

--------------------------------------------------------------------------------
onJsonImport :: DhekProgram ()
onJsonImport = compile $ do
    fOpt <- openJsonFile
    traverse_ go fOpt
  where
    go path = do
        ebytes <- performIO $ try $ B.readFile path
        case ebytes of
            Left e      -> exception e
            Right bytes -> do
                let rectsE = fmap saveToRects (eitherDecode bytes)
                either showError upd rectsE

    upd rs = do
        setRects rs
        draw

--------------------------------------------------------------------------------
exception (e :: SomeException) = showError $ show e

--------------------------------------------------------------------------------
saveToRects :: Save -> [(Int, [Rect])]
saveToRects (Save _ xs) = fmap go xs
  where
    go (i, xsOpt) = (i, maybe [] id xsOpt)
