--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine
--
--
--------------------------------------------------------------------------------
module Dhek.Engine
    ( Collision(..)
    , DrawEnv(..)
    , EngineState
    , Interpreter
    , M
    , Pos
    , drawInterpret
    , engineDrawingArea
    , engineCurrentPage
    , engineCurrentState
    , engineRunDraw
    , engineSetMode
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
    , makeInterpreter
    , runProgram
    ) where

--------------------------------------------------------------------------------
import Dhek.Engine.Interpreter
import Dhek.Engine.Type
