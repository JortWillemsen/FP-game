module Model.Move where

import Model.Constants
import Model.Collidable (Collidable, collides, collidesWith, collidesWithTag)
import Debug.Trace (trace)
import Data.List (sort, sortBy)
import Data.Function (on)

data Toggled = Depressed | Released deriving (Eq)

type InputBuffer = (Char, Toggled, Direction)

type Position = (Float, Float)

type Speed = Float

data Direction = L | R | U | D deriving (Eq, Show)

-- | Inverses the direction
inverse :: Direction -> Direction
inverse L = R
inverse R = L
inverse U = D
inverse D = U

class Moveable a where
  move :: a -> Direction -> Speed -> a
  moveTo :: a -> Position -> a
  pos :: a -> Position
  dir :: a -> Direction

-- | Moves the moveable up
up :: (Moveable a) => a -> Speed -> Position
up m s = let (x, y) = pos m in (x, y + s)

-- | Moves the moveable down
down :: (Moveable a) => a -> Speed -> Position
down m s = let (x, y) = pos m in (x, y - s)

-- | Moves the moveable left
left :: (Moveable a) => a -> Speed -> Position
left m s = let (x, y) = pos m in (x - s, y)

-- | Moves the moveable right
right :: (Moveable a) => a -> Speed -> Position
right m s = let (x, y) = pos m in (x + s, y)

-- | Finds distance between two points using the Pythagorean theorem
pythagoras :: Position -> Position -> Float
pythagoras (x, y) (x', y') = sqrt (a + b) where
  a = abs (x' - x) **2
  b = abs (y' - y) **2

-- | Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a, Collidable b) => a -> Direction -> [b] -> Maybe a
tryMove m d cs = if any (collidesWithTag ["wall", "trapdoor"] $ move m d speed) cs 
  then Nothing
  else Just (move m d speed)
