module Ghost where
import Maze ( Maze, getNeighbouringTiles, Tile(Wall, Floor), getNeighbouringFloorTiles )
import Move ( Position, Direction )

data Ghost = Blinky Position Direction
            | Pinky Position Direction
            | Inky  Position Direction
            | Clyde Position Direction

