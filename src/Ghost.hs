module Ghost where
import Maze ( Maze, getNeighbouringTiles, Tile(Wall, Floor), getNeighbouringFloorTiles )
import Move ( Position, Direction )
import Constants

data Ghost = Blinky Position Direction
            | Pinky Position Direction
            | Inky  Position Direction
            | Clyde Position Direction

instance Moveable Ghost where
  move (Ghost (x, y) d) L = Ghost (x - speed, y) L
  move (Ghost (x, y) d) R = Ghost (x + speed, y) R
  move (Ghost (x, y) d) U = Ghost (x, y + speed) U
  move (Ghost (x, y) d) D = Ghost (x, y - speed) D

instance Collidable Ghost where
  hitBox (Ghost p(x, y) _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]