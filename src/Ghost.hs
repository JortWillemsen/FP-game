module Ghost where
import Player ( Player(PuckMan) )
import Maze
import Move 

data Ghost = Blinky Position
            | Pinky Position
            | Inky Position
            | Clyde Position

moveAlgorithm :: Ghost -> Player -> Ghost
moveAlgorithm (Blinky (xg, yg)) (PuckMan (xp, yp) _ _) | xp == xg && yg == yp             = Blinky (xg, yg)
                                                       | yg == yp                         = Blinky (x', yg)
                                                       | (xp - 1) == xg || xg == (xp + 1) = Blinky (xg, y')
                                                       | otherwise                        = Blinky (x', y')
  where
    x' | xp <= xg  = xg - 1
       | otherwise = xg + 1
    y' | yp >= yg  = yg + 1
       | otherwise = yg - 1