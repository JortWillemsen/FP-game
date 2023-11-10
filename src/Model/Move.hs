module Model.Move where

import Model.Constants
import Model.Collidable (Collidable, collides, collidesWith)
import Debug.Trace (trace)
import Data.List (sort, sortBy)
import Data.Function (on)

data Toggled = Depressed | Released deriving (Eq)

type InputBuffer = (Char, Toggled, Direction)

type Position = (Float, Float)

type Speed = Float

type Move = (Position, Position)

data Direction = L | R | U | D deriving (Eq, Show)

inverse :: Direction -> Direction
inverse L = R
inverse R = L
inverse U = D
inverse D = U

class Moveable a where
  move :: a -> Direction -> Speed -> a
  moveTo :: a -> Position -> a
  pos :: a -> Position
  buffer :: a -> [InputBuffer]
  dir :: a -> Direction

up :: (Moveable a) => a -> Speed -> Position
up m s = let (x, y) = pos m in (x, y + s)

down :: (Moveable a) => a -> Speed -> Position
down m s = let (x, y) = pos m in (x, y - s)

left :: (Moveable a) => a -> Speed -> Position
left m s = let (x, y) = pos m in (x - s, y)

right :: (Moveable a) => a -> Speed -> Position
right m s = let (x, y) = pos m in (x + s, y)

manhattan :: Position -> Position -> Float
manhattan (x, y) (x', y') = sqrt (a + b) where
  a = abs (x' - x) **2
  b = abs (y' - y) **2

-- Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a, Collidable b) => a -> Direction -> [b] -> Maybe a
tryMove m d cs = if any (\x -> collidesWith (move m d speed) x ["wall"]) cs 
  then Nothing
  else Just (move m d speed)
