{-# LANGUAGE InstanceSigs #-} -- IDK wat dit doet, moet ik nog ff uitzoeken
module Player where

import Moveable ( Position, Moveable (getPosition, up, down, left, right) )

data Player = PuckMan Position
            | MsPuckMan Position
            | JrPuckMan Position
            | BabyPuckMan Position

instance Moveable Player where 
    getPosition :: Player -> Position -- Get player's position
    getPosition (PuckMan p) = p 
    getPosition (MsPuckMan p) = p 
    getPosition (JrPuckMan p) = p 
    getPosition (BabyPuckMan p) = p 

    up :: Player -> Player -- Move player up
    up (PuckMan (x, y)) = PuckMan (x, y+5)
    up (MsPuckMan (x, y)) = MsPuckMan (x, y+5)
    up (JrPuckMan (x, y)) = JrPuckMan (x, y+5) 
    up (BabyPuckMan (x, y)) = BabyPuckMan (x, y+5) 

    down :: Player -> Player -- Move player down
    down (PuckMan (x, y)) = PuckMan (x, y-5)
    down (MsPuckMan (x, y)) = MsPuckMan (x, y-5)
    down (JrPuckMan (x, y)) = JrPuckMan (x, y-5) 
    down (BabyPuckMan (x, y)) = BabyPuckMan (x, y-5) 

    left :: Player -> Player -- Move player left
    left (PuckMan (x, y)) = PuckMan (x-5, y)
    left (MsPuckMan (x, y)) = MsPuckMan (x-5, y)
    left (JrPuckMan (x, y)) = JrPuckMan (x-5, y) 
    left (BabyPuckMan (x, y)) = BabyPuckMan (x-5, y) 

    right :: Player -> Player -- Move player right
    right (PuckMan (x, y)) = PuckMan (x+5, y)
    right (MsPuckMan (x, y)) = MsPuckMan (x+5, y)
    right (JrPuckMan (x, y)) = JrPuckMan (x+5, y) 
    right (BabyPuckMan (x, y)) = BabyPuckMan (x+5, y) 