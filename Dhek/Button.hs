module Dhek.Button where

import Data.Foldable (traverse_)

import Dhek.Instr

onPrev :: DhekProgram ()
onPrev = compile $ do
    decrPage
    i    <- getCurPage
    nb   <- getPageCount
    name <- getFilename
    setTitle (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
    draw

onNext :: DhekProgram ()
onNext = compile $ do
    incrPage
    i    <- getCurPage
    nb   <- getPageCount
    name <- getFilename
    setTitle (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
    draw

onMinus :: DhekProgram ()
onMinus = compile $ do
    decrZoom
    draw

onPlus :: DhekProgram ()
onPlus = compile $ do
    incrZoom
    draw

onRem :: DhekProgram ()
onRem = compile $ do
    sOpt <- getSelected
    traverse_ go sOpt
  where
    go r = do
        removeRect r
        unselectRect
        draw
