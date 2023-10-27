module Model.Move where

import Model.Constants
import Model.Collidable (Collidable, collides)
import Debug.Trace (trace)

data Toggled = Depressed | Released deriving (Eq)

type InputBuffer = (Char, Toggled, Direction)

type Position = (Float, Float)

type Move = (Position, Position)

data Direction = L | R | U | D deriving Eq

class Moveable a where
  move :: a -> Direction -> a
  pos :: a -> Position
  buffer :: a -> [InputBuffer]
  dir :: a -> Direction

up :: (Moveable a) => a -> Position
up m = let (x, y) = pos m in (x, y + speed)

down :: (Moveable a) => a -> Position
down m = let (x, y) = pos m in (x, y - speed)

left :: (Moveable a) => a -> Position
left m = let (x, y) = pos m in (x - speed, y)

right :: (Moveable a) => a -> Position
right m = let (x, y) = pos m in (x + speed, y)

translateMovable :: (Moveable a, Collidable a, Collidable b) => a -> [b] -> a
translateMovable m cs = 
  let (_, _, d) = head $ filter (\(_, t, a) -> t == Depressed) (buffer m) in
    case tryMove m d cs of 
      Nothing -> case tryMove m (dir m) cs of
        Nothing -> m
        Just p' -> p'
      Just p -> p

-- Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a, Collidable b) => a -> Direction -> [b] -> Maybe a
tryMove m d cs = if any (\x -> move m d `collides` x) cs 
  then Nothing
  else Just (move m d)
    
