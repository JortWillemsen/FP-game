module Model.Move where

import Model.Collidable
import Model.Constants
import Model.Maze (Maze)

type Position = (Float, Float)

type Move = (Position, Position)

data Direction = L | R | U | D deriving Eq

class Moveable a where
  move :: a -> a
  pos :: a -> Position

up :: (Moveable a) => a -> Position
up m = let (x, y) = pos m in (x, y + speed)

down :: (Moveable a) => a -> Position
down m = let (x, y) = pos m in (x, y - speed)

left :: (Moveable a) => a -> Position
left m = let (x, y) = pos m in (x - speed, y)

right :: (Moveable a) => a -> Position
right m = let (x, y) = pos m in (x + speed, y)

-- Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a) => a -> [a] -> Maybe a
tryMove m cs = if doesCollide
  then Nothing
  else Just $ move m where
    doesCollide :: Bool
    doesCollide = any (move m `collides` ) cs
