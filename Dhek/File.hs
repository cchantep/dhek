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
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
import           Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           System.FilePath (takeExtension)

--------------------------------------------------------------------------------
import Dhek.Free
import Dhek.Instr
import Dhek.Types

--------------------------------------------------------------------------------
onJsonSave :: DhekProgram Bool
onJsonSave = compile $ do
    fOpt <- selectJsonFile
    r    <- traverse go fOpt
    return $ maybe False id r
  where
    go path = do
        rs <- getAllRects
        let nb   = length rs
            save = saveNew $ fillUp nb rs
            path1
                | takeExtension path == ".json" = path
                | otherwise                     = path ++ ".json"

        e <- performIO $ try $ B.writeFile path1 (encode save)
        either (\x -> exception x >> return False)
            (const (clearEvents >> return True)) e

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
        clearEvents
        setRects rs
        draw

--------------------------------------------------------------------------------
exception (e :: SomeException) = showError $ show e

--------------------------------------------------------------------------------
saveToRects :: Save -> [(Int, [Rect])]
saveToRects (Save _ xs)
    = fmap (\(i, rs) -> (i, expandGrouped rs)) xs
