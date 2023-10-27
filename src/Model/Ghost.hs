module Model.Ghost where

import Model.Collidable (Collidable (hitBox), HitBox)
import Model.Constants
import Model.Maze (Maze, Tile (Floor, Wall), getNeighbouringFloorTiles, getNeighbouringTiles)
import Model.Move

data GhostType = Blinky | Pinky | Inky | Clyde

data Ghost = Ghost {
  ghostType :: GhostType,
  position :: Position,
  direction :: Direction
}

instance Moveable Ghost where
  pos (Ghost _ p _) = p
  move (Ghost t (x, y) _) d
    | d == U = Ghost t (x - speed, y) d
    | d == D = Ghost t (x + speed, y) d
    | d == L = Ghost t (x, y - speed) d
    | d == R = Ghost t (x, y + speed) d

instance Collidable Ghost where
  hitBox (Ghost _ p@(x, y) _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]