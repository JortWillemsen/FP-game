module Move where
import Maze (Maze)
import Collidable

type Position = (Float, Float)
type Move = (Position, Position)
data Direction = L | R | U | D -- mb nesw
data Association = GoUp | GoDown | GoLeft | GoRight deriving Eq

class Moveable a where
  move :: a -> Direction -> a
  pos  :: a -> Position

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

up :: Moveable a => a -> Position
up (x, y) = (x, y + speed)

down :: Moveable a => a -> Position
down (x, y) = (x, y - speed)

left :: Moveable a => a -> Position
left (x, y) = (x - speed, y)

right :: Moveable a => a -> Position
right (x, y) = (x + speed, y)

-- Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a b) => a -> [b] -> a
tryMove m collisions
  | any (moved `collides`) collisions = Nothing
  | otherwise                         = Just moved 
  where
    moved = move m