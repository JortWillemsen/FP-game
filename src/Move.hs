module Move where

type Position = (Float, Float)

type Move = (Position, Position)
data Direction = L | R | U | D -- mb nesw
data Association = GoUp | GoDown | GoLeft | GoRight 
                   deriving Eq

speed :: Float
speed = 2

moveToDirection :: Association -> Direction -- association to direction
moveToDirection GoRight = R 
moveToDirection GoLeft = L 
moveToDirection GoUp = U 
moveToDirection GoDown = D

directionToMove :: Direction -> Association
directionToMove R = GoRight 
directionToMove L = GoLeft 
directionToMove U = GoUp 
directionToMove D = GoDown

-- Gets the associated move function for a move association
getMove :: Association -> (Position -> Position)
getMove a
  | a == GoUp = up
  | a == GoDown = down
  | a == GoLeft = left
  | a == GoRight = right

up :: Position -> Position
up (x, y) = (x, y + speed)

down :: Position -> Position
down (x, y) = (x, y - speed)

left :: Position -> Position
left (x, y) = (x - speed, y)

right :: Position -> Position
right (x, y) = (x + speed, y)
