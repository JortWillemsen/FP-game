module Model.Ghost where

import Model.Collidable
import Model.Constants
import Model.Maze (Maze, Tile (Floor, Wall), getNeighbouringFloorTiles, getNeighbouringTiles)
import Model.Move

data Ghost
  = Blinky Position Direction
  | Pinky Position Direction
  | Inky Position Direction
  | Clyde Position Direction

instance Moveable Ghost where
  pos (Blinky p _) = p
  move (Blinky (x, y) d)
    | d == U = Blinky (x - speed, y) d
    | d == D = Blinky (x + speed, y) d
    | d == L = Blinky (x, y - speed) d
    | d == R = Blinky (x, y + speed) d

instance Collidable Ghost where
  hitBox (Blinky p@(x, y) _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]