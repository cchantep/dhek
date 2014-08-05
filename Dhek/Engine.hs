--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine
--
--
--------------------------------------------------------------------------------
module Dhek.Engine
    ( Collision(..)
    , DrawEnv(..)
    , DhekMode(..)
    , EngineState
    , RuntimeEnv
    , Pos
    , engineDrawingArea
    , engineCurrentPage
    , engineCurrentState
    , engineModeMove
    , engineModePress
    , engineModeRelease
    , engineModeDraw
    , engineModeKeyPress
    , engineModeKeyRelease
    , engineModeEnter
    , engineModeLeave
    , engineSetMode
    , engineHasEvents
      -- Draw lenses
    , drawCollision
    , drawCursor
    , drawEvent
    , drawFreshId
    , drawOverRect
    , drawSelected
    , drawSelection
      -- Engine lenses
    , engineBoards
    , engineDrawState
      --
    , engineRatio
    , getRects
    , loadPdfFileDoc
    , loadPdfInlinedDoc
    , loadViewer
    , makeRuntimeEnv
    , makeViewer
    , runProgram
    ) where

--------------------------------------------------------------------------------
import Dhek.Engine.Runtime
import Dhek.Engine.Type
