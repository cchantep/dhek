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
    , M
    , Pos
    , drawInterpret
    , engineDrawingArea
    , engineCurrentPage
    , engineCurrentState
    , engineRunDraw
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
      -- Mode API
    , move
    , press
    , release
    , drawing
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
