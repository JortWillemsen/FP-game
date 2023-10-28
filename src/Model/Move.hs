module Model.Move where

import Model.Constants
import Model.Collidable (Collidable, collides)
import Debug.Trace (trace)
import Data.List (sort, sortBy)
import Data.Function (on)

data Toggled = Depressed | Released deriving (Eq)

type InputBuffer = (Char, Toggled, Direction)

type Position = (Float, Float)

type Move = (Position, Position)

data Direction = L | R | U | D deriving (Eq, Show)

inverse :: Direction -> Direction
inverse L = R
inverse R = L
inverse U = D
inverse D = U

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

translatePlayer :: (Moveable a, Collidable a, Collidable b) => a -> [b] -> a
translatePlayer m cs = 
  let (_, _, d) = head $ filter (\(_, t, a) -> t == Depressed) (buffer m) in
    case tryMove m d cs of 
      Nothing -> case tryMove m (dir m) cs of
        Nothing -> m
        Just p' -> p'
      Just p -> p

translateGhost :: (Moveable a, Collidable a, Collidable c) => a -> Position -> [c] -> a
translateGhost g p cs = if length sortedMoves < 1
  then move g (inverse $ dir g)
  else fst $ head sortedMoves 
    where
      sortedMoves = sortBy (compare `on` snd) possibleMoves
      possibleMoves = foldr f [] movesPerDir
      f (Just x) r = (x, manhattan (pos x) p) : r
      f Nothing r = r
      movesPerDir = case dir g of
        L -> [tryMove g D cs, tryMove g L cs, tryMove g U cs]
        R -> [tryMove g D cs, tryMove g R cs, tryMove g U cs]
        U -> [tryMove g L cs, tryMove g U cs, tryMove g R cs]
        D -> [tryMove g L cs, tryMove g D cs, tryMove g R cs]

manhattan :: Position -> Position -> Float
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

-- Takes a movable with a direction and a list of all possible collisions to check if the move is valid
tryMove :: (Moveable a, Collidable a, Collidable b) => a -> Direction -> [b] -> Maybe a
tryMove m d cs = if any (\x -> move m d `collides` x) cs 
  then Nothing
  else Just (move m d)
    
