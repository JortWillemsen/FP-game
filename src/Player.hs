module Player where

import Maze
import Move ( Association(..), Position, Direction(L) )

data Player = Player { playerType :: PlayerType
                     , position :: Position
                     , inputBuffer :: [InputBuffer]
                     , direction :: Direction
                     }

data PlayerType = PuckMan 
                | MsPuckMan 
                | JrPuckMan 
                | BabyPuckMan  

data Toggled = Depressed | Released 
               deriving Eq

type InputBuffer = (Char, Toggled, Association)

resetInputBuffer :: Player -> Player
resetInputBuffer (Player _ pos ibs L) = Player PuckMan pos inputBufferWASD L

inputBufferWASD :: [InputBuffer]
inputBufferWASD = [('w', Released, GoUp), 
                   ('a', Released, GoLeft), 
                   ('s', Released, GoDown), 
                   ('d', Released, GoRight)]


