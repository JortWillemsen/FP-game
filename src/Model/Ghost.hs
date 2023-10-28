module Model.Ghost where

import Model.Collidable (Collidable (hitBox), HitBox)
import Model.Constants
import Model.Maze (Maze, Tile (Floor, Wall), getNeighbouringFloorTiles, getNeighbouringTiles)
import Model.Move

data GhostType = Blinky | Pinky | Inky | Clyde

data Ghost = Ghost {
  ghostType :: GhostType,
  position :: Position,
  direction :: Direction,
  buffer :: [InputBuffer]
}

instance Moveable Ghost where
  pos (Ghost _ p _ _) = p
  buffer (Ghost _ _ _ b) = b
  dir (Ghost _ _ d _) = d
  move (Ghost t (x, y) _ b) d
    | d == U = Ghost t (x - speed, y) d b
    | d == D = Ghost t (x + speed, y) d b
    | d == L = Ghost t (x, y - speed) d b
    | d == R = Ghost t (x, y + speed) d b
  

instance Collidable Ghost where
  hitBox (Ghost _ p@(x, y) _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]