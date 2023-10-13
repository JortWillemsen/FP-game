module Player where

import Move ( Position, Association (..) )

data Player = PuckMan Position [InputBuffer]
            | MsPuckMan Position [InputBuffer]
            | JrPuckMan Position [InputBuffer]
            | BabyPuckMan Position [InputBuffer]

data Toggled = Depressed | Released 
               deriving Eq

type InputBuffer = (Char, Toggled, Association)

resetInputBuffer :: Player -> Player
resetInputBuffer (PuckMan pos ibs) = PuckMan pos inputBufferWASD

inputBufferWASD :: [InputBuffer]
inputBufferWASD = [('w', Released, GoUp), 
                   ('a', Released, GoLeft), 
                   ('s', Released, GoDown), 
                   ('d', Released, GoRight)]

-- inputBufferArrows :: [InputBuffer]
-- inputBufferArrows = [('w', Released, GoUp), 
--                      ('a', Released, GoLeft), 
--                      ('s', Released, GoDown), 
--                      ('d', Released, GoRight)]

-- instance Moveable Player where 
--     getPosition :: Player -> Position -- Get player's position
--     getPosition (PuckMan p _) = p 
--     getPosition (MsPuckMan p _) = p 
--     getPosition (JrPuckMan p _) = p 
--     getPosition (BabyPuckMan p _) = p 

--     up :: Player -> Player -- Move player up
--     up (PuckMan (x, y) ib) = PuckMan (x, y+5) ib
--     up (MsPuckMan (x, y) ib) = MsPuckMan (x, y+5) ib
--     up (JrPuckMan (x, y) ib) = JrPuckMan (x, y+5) ib
--     up (BabyPuckMan (x, y) ib) = BabyPuckMan (x, y+5) ib

--     down :: Player -> Player -- Move player down
--     down (PuckMan (x, y) ib) = PuckMan (x, y-5) ib
--     down (MsPuckMan (x, y) ib) = MsPuckMan (x, y-5) ib
--     down (JrPuckMan (x, y) ib) = JrPuckMan (x, y-5) ib
--     down (BabyPuckMan (x, y) ib) = BabyPuckMan (x, y-5) ib

--     left :: Player -> Player -- Move player left
--     left (PuckMan (x, y) ib) = PuckMan (x-5, y) ib
--     left (MsPuckMan (x, y) ib) = MsPuckMan (x-5, y) ib
--     left (JrPuckMan (x, y) ib) = JrPuckMan (x-5, y) ib
--     left (BabyPuckMan (x, y) ib) = BabyPuckMan (x-5, y) ib

--     right :: Player -> Player -- Move player right
--     right (PuckMan (x, y) ib) = PuckMan (x+5, y) ib
--     right (MsPuckMan (x, y) ib) = MsPuckMan (x+5, y) ib
--     right (JrPuckMan (x, y) ib) = JrPuckMan (x+5, y) ib
--     right (BabyPuckMan (x, y) ib) = BabyPuckMan (x+5, y) ib