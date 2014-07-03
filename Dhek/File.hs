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
import           Control.Monad.Trans (liftIO)
import           Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           System.FilePath (takeExtension)

--------------------------------------------------------------------------------
import Dhek.Engine.Instr
import Dhek.Types

--------------------------------------------------------------------------------
onJsonSave :: Instr Bool
onJsonSave =
    do fOpt <- selectJsonFile
       r    <- traverse go fOpt
       return $ maybe False id r
  where
    go path =
        do rs <- getAllRects
           let nb   = length rs
               save = saveNew $ fillUp nb rs
               path1
                   | takeExtension path == ".json" = path
                   | otherwise                     = path ++ ".json"

           e <- liftIO $ try $ B.writeFile path1 (encode save)
           either (\x -> exception x >> return False)
               (const (clearEvents >> return True)) e

--------------------------------------------------------------------------------
onJsonImport :: Instr ()
onJsonImport
    = do fOpt <- openJsonFile
         traverse_ go fOpt
  where
    go path
        = do ebytes <- liftIO $ try $ B.readFile path
             case ebytes of
                 Left e      -> exception e
                 Right bytes ->
                     do let rectsE = fmap saveToRects (eitherDecode bytes)
                        either showError upd rectsE

    upd rs
        = do clearEvents
             setAllRects rs
             draw

--------------------------------------------------------------------------------
exception :: SomeException -> Instr ()
exception (e :: SomeException) = showError $ show e

--------------------------------------------------------------------------------
saveToRects :: Save -> [(Int, [Rect])]
saveToRects (Save _ xs)
    = fmap (\(i, rs) -> (i, expandGrouped rs)) xs
