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
    , engineSetMode
    , engineHasEvents
    , engineIsNormalMode
    , engineRevertMode
    , engineIsDuplicateCtrlMode
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
    , loadPdf
    , makeRuntimeEnv
    , runProgram
    ) where

--------------------------------------------------------------------------------
import Dhek.Engine.Runtime
import Dhek.Engine.Type
