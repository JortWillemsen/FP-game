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
  move (Blinky (x, y) d) L = Blinky (x - speed, y) L
  move (Blinky (x, y) d) R = Blinky (x + speed, y) R
  move (Blinky (x, y) d) U = Blinky (x, y + speed) U
  move (Blinky (x, y) d) D = Blinky (x, y - speed) D
  pos (Blinky p _) = p

instance Collidable Ghost where
  hitBox (Blinky p@(x, y) _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]