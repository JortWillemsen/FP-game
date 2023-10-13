module Ghost where
import Move ( Position )
import Player ( Player(PuckMan) )

data Ghost = Blinky Position
            | Pinky Position
            | Inky Position
            | Clyde Position

moveAlgorithm :: Ghost -> Player -> Ghost
moveAlgorithm (Blinky (xg, yg)) (PuckMan (xp, yp) _) | xp == xg && yg == yp                 = Blinky (xg, yg)
                                                     | yg == yp                             = Blinky (x', yg)
                                                     | (xp - 2.5) == xg || xg == (xp + 2.5) = Blinky (xg, y')
                                                     | otherwise                            = Blinky (x', y')
  where
    x' | xp <= xg  = xg - 2.5 
       | otherwise = xg + 2.5
    y' | yp >= yg  = yg + 2.5 
       | otherwise = yg - 2.5