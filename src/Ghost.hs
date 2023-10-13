module Ghost where
import Move ( Position )
import Player ( Player(PuckMan) )

data Ghost = Blinky Position
            | Pinky Position
            | Inky Position
            | Clyde Position

moveAlgorithm :: Ghost -> Player -> Ghost
moveAlgorithm (Blinky (xg, yg)) (PuckMan (xp, yp) _) = 
    Blinky (x', y')
  where
    x' = if xp < xg 
          then xg - 2 
          else xg + 2 
    y' = if yp > yg 
          then yg + 2 
          else yg - 2