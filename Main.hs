module Main where

import Control.Lens ((.=))
import Control.Monad.State (execState)

import Dhek.Button (onNext, onPrev, onMinus, onPlus, onRem)
import Dhek.Draw (cairoDraw)
import Dhek.Engine
import Dhek.File (onJsonSave, onJsonImport)
import Dhek.Move (onMove, onPress, onRelease)
import Dhek.Property (onProp)
import Dhek.Selection (onSel)
import Dhek.I18N

main :: IO ()
main = do
    eng <- fmap (execState conf) gtkEngineNew
    engineStart eng
  where
    conf = do
        engineDrawing       .= cairoDraw
        engineMove          .= onMove
        enginePress         .= onPress
        engineRelease       .= onRelease
        engineNextPage      .= onNext
        enginePrevPage      .= onPrev
        engineNextZoom      .= onPlus
        enginePrevZoom      .= onMinus
        engineTreeSelection .= onSel
        engineRemoveRect    .= onRem
        enginePropChanged   .= onProp
        engineJsonSave      .= onJsonSave
        engineJsonLoad      .= onJsonImport
