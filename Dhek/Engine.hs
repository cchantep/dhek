--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine
--
--
--------------------------------------------------------------------------------
module Dhek.Engine
    ( Collision(..)
    , DrawOptions(..)
    , EngineState
    , Interpreter
    , M
    , Pos
    , drawInterpret
    , engineDrawingArea
    , engineCurrentPage
    , engineCurrentState
    , engineSetMode
      -- Draw lenses
    , drawAttached
    , drawCollision
    , drawCursor
    , drawDetached
    , drawEvent
    , drawFreshId
    , drawNewRect
    , drawSelected
    , drawSelection
      -- Engine lenses
    , engineBoards
    , engineDrawState
      -- Mode API
    , selectRectangle
    , unselectRectangle
    , getDrawSelection
    , setDrawSelection
    , getEvent
    , setEvent
    , getCollision
    , setCollision
    , newRectangle
    , attachRectangle
    , detachRectangle
    , setCursor
    , freshId
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
