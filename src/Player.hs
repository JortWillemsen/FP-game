module Player where

import Maze
import Move

data Direction = L | R | U | D 

data Player = PuckMan Position [InputBuffer] Direction 
            | MsPuckMan Position [InputBuffer] Direction
            | JrPuckMan Position [InputBuffer] Direction 
            | BabyPuckMan Position [InputBuffer] Direction 

data Toggled = Depressed | Released 
               deriving Eq

type InputBuffer = (Char, Toggled, Association)

resetInputBuffer :: Player -> Player
resetInputBuffer (PuckMan pos ibs L) = PuckMan pos inputBufferWASD L

inputBufferWASD :: [InputBuffer]
inputBufferWASD = [('w', Released, GoUp), 
                   ('a', Released, GoLeft), 
                   ('s', Released, GoDown), 
                   ('d', Released, GoRight)]


