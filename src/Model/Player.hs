{-# LANGUAGE InstanceSigs #-}
module Model.Player where

import Model.Collidable (Collidable (collisions, hitBox, name))
import Model.Constants
import Model.Maze (Maze)
import Model.Move

data Player = Player
  { playerType :: PlayerType,
    position :: Position,
    inputBuffer :: [InputBuffer],
    direction :: Direction,
    spawnPoint :: Position
  }
  deriving (Eq)

data PlayerType
  = PuckMan
  | MsPuckMan
  | JrPuckMan
  | BabyPuckMan
  deriving (Eq)

instance Show PlayerType where 
  show PuckMan = "Puck-Man"
  show MsPuckMan = "Ms. Puck-Man"
  show JrPuckMan = "Jr. Puck-Man"
  show BabyPuckMan = "Baby Puck-Man"

instance Moveable Player where
  move p@(Player t (x, y) i _ sp) d s
    | d == U = Player t (up p s) i d sp
    | d == D = Player t (down p s) i d sp
    | d == L = Player t (left p s) i d sp
    | d == R = Player t (right p s) i d sp
  moveTo (Player t p i d sp) newP = Player t newP i d sp
  pos (Player _ p _ _ _) = p
  buffer (Player _ _ i _ _) = i
  dir (Player _ _ _ d _) = d

instance Collidable Player where
  name (Player {}) = "player"
  collisions (Player {}) = ["ghost", "wall", "trapdoor", "collectible"]
  hitBox (Player _ p@(x, y) _ _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

resetInputBuffer :: Player -> Player
resetInputBuffer (Player t pos ibs L sp) = Player t pos inputBufferWASD L sp

inputBufferWASD :: [InputBuffer]
inputBufferWASD =
  [ ('w', Released, U),
    ('a', Depressed, L),
    ('s', Released, D),
    ('d', Released, R)
  ]

translatePlayer :: (Collidable a) => Player -> [a] -> Player
translatePlayer m cs =
  let (_, _, d) = head $ filter (\(_, t, a) -> t == Depressed) (buffer m)
   in case tryMove m d cs of
        Nothing -> case tryMove m (dir m) cs of
          Nothing -> m
          Just p' -> p'
        Just p -> p