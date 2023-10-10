-- | This module contains the data types
--   which represent the state of the game
module Model where

import Moveable
import Player

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

ticksPerSec :: Float
ticksPerSec = 0.03

startPos :: Position 
startPos = Pos (0, 0)

data Model = State {
  player :: Player
}

initialState :: Model
initialState = State (PuckMan startPos)