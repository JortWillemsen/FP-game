module Model.Player where

import Model.Collidable (Collidable (hitBox, collisions, name))
import Model.Constants
import Model.Maze (Maze)
import Model.Move

data Player = Player
  { playerType :: PlayerType,
    position :: Position,
    inputBuffer :: [InputBuffer],
    direction :: Direction
  } deriving (Eq)

data PlayerType
  = PuckMan
  | MsPuckMan
  | JrPuckMan
  | BabyPuckMan deriving (Eq)

instance Moveable Player where
  move p@(Player t (x, y) i _) d s
    | d == U = Player t (up p s) i d
    | d == D = Player t (down p s ) i d
    | d == L = Player t (left p s) i d
    | d == R = Player t (right p s) i d
  pos (Player _ p _ _) = p
  buffer (Player _ _ i _) = i
  dir (Player _ _ _ d) = d


instance Collidable Player where
  name (Player {}) = "player"
  collisions (Player {}) = ["ghost", "wall", "trapdoor"]
  hitBox (Player _ p@(x, y) _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

resetInputBuffer :: Player -> Player
resetInputBuffer (Player _ pos ibs L) = Player PuckMan pos inputBufferWASD L

inputBufferWASD :: [InputBuffer]
inputBufferWASD =
  [ ('w', Released, U),
    ('a', Depressed, L),
    ('s', Released, D),
    ('d', Released, R)
  ]
