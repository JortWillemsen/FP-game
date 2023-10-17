module Move where

type Position = (Float, Float)
type Move = (Position, Position)

data Association = GoUp | GoDown | GoLeft | GoRight 
                   deriving Eq

-- Gets the associated move function for a move association
getMove :: Association -> (Position -> Position)
getMove a | a == GoUp = up
          | a == GoDown = down
          | a == GoLeft = left
          | a == GoRight = right

up :: Position -> Position
up (x, y) = (x, y+5)

down :: Position -> Position 
down (x, y) = (x, y-5)

left :: Position -> Position
left (x, y) = (x-5, y)

right :: Position -> Position 
right (x, y) = (x+5, y)
