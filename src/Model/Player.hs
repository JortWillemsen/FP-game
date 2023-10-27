module Model.Player where

import Model.Maze (Maze)
import Model.Move
import Model.Constants
import Model.Collidable

data Toggled = Depressed | Released deriving Eq
type InputBuffer = (Char, Toggled, Direction)

data Player = Player { playerType :: PlayerType
                     , position :: Position
                     , inputBuffer :: [InputBuffer]
                     , direction :: Direction
                     }

data PlayerType = PuckMan 
                | MsPuckMan 
                | JrPuckMan 
                | BabyPuckMan  

instance Moveable Player where
  move (Player t (x, y) i d) L = Player t (x - speed, y) i L
  move (Player t (x, y) i d) R = Player t (x + speed, y) i R
  move (Player t (x, y) i d) U = Player t (x, y + speed) i U
  move (Player t (x, y) i d) D = Player t (x, y - speed) i D
  pos (Player _ p _ _) = p

instance Collidable Player where
  hitBox (Player _ p@(x, y) _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

resetInputBuffer :: Player -> Player
resetInputBuffer (Player _ pos ibs L) = Player PuckMan pos inputBufferWASD L

inputBufferWASD :: [InputBuffer]
inputBufferWASD = [('w', Released, U), 
                   ('a', Released, L), 
                   ('s', Released, D), 
                   ('d', Released, R)]
