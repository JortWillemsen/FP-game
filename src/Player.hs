module Player where

import Maze
import Move

data Player = PuckMan Position [InputBuffer]
            | MsPuckMan Position [InputBuffer]
            | JrPuckMan Position [InputBuffer]
            | BabyPuckMan Position [InputBuffer]

data Toggled = Depressed | Released 
               deriving Eq

type InputBuffer = (Char, Toggled, Association)

resetInputBuffer :: Player -> Player
resetInputBuffer (PuckMan pos ibs) = PuckMan pos inputBufferWASD

inputBufferWASD :: [InputBuffer]
inputBufferWASD = [('w', Released, GoUp), 
                   ('a', Released, GoLeft), 
                   ('s', Released, GoDown), 
                   ('d', Released, GoRight)]


