--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Action
--
--------------------------------------------------------------------------------
module Dhek.Action where

--------------------------------------------------------------------------------
import Data.Foldable (for_)

--------------------------------------------------------------------------------
import Dhek.AppUtil (browserOpen)
import Dhek.Engine.Instr

--------------------------------------------------------------------------------
onPrev :: Instr ()
onPrev
    = do decrPage
         i    <- getCurrentPage
         nb   <- getPageCount
         name <- getFilename
         setTitle (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
         draw

--------------------------------------------------------------------------------
onNext :: Instr ()
onNext
    = do incrPage
         i    <- getCurrentPage
         nb   <- getPageCount
         name <- getFilename
         setTitle (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
         draw

--------------------------------------------------------------------------------
onMinus :: Instr ()
onMinus
    = do decrZoom
         draw

--------------------------------------------------------------------------------
onPlus :: Instr ()
onPlus
    = do incrZoom
         draw

--------------------------------------------------------------------------------
onRem :: Instr ()
onRem
    = do rs <- getAllSelected
         for_ rs $ \r ->
             do removeRect r
                addEvent DeleteRect
         unselectRect
         draw

--------------------------------------------------------------------------------
onApplidok :: IO ()
onApplidok = browserOpen "http://go.applidok.com"
