module Moveable where

type Position = (Float, Float)
type Move = (Position, Position)

class Moveable a where 
    getPosition :: a -> Position
    up :: a -> a
    down :: a -> a
    left :: a -> a 
    right :: a -> a
