module Model.Player where

import Model.Collidable
import Model.Constants
import Model.Maze (Maze)
import Model.Move

data Toggled = Depressed | Released deriving (Eq)

type InputBuffer = (Char, Toggled, Direction)

data Player = Player
  { playerType :: PlayerType,
    position :: Position,
    inputBuffer :: [InputBuffer],
    direction :: Direction
  }

data PlayerType
  = PuckMan
  | MsPuckMan
  | JrPuckMan
  | BabyPuckMan

instance Moveable Player where
  move p@(Player t (x, y) i d)
    | d == U = Player t (up p) i d
    | d == D = Player t (down p) i d
    | d == L = Player t (left p) i d
    | d == R = Player t (right p) i d
  pos (Player _ p _ _) = p

instance Collidable Player where
  hitBox (Player _ p@(x, y) _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

resetInputBuffer :: Player -> Player
resetInputBuffer (Player _ pos ibs L) = Player PuckMan pos inputBufferWASD L

inputBufferWASD :: [InputBuffer]
inputBufferWASD =
  [ ('w', Released, U),
    ('a', Released, L),
    ('s', Released, D),
    ('d', Released, R)
  ]
