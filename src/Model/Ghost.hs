module Model.Ghost where

import Model.Collidable (Collidable (hitBox, collisions, name), HitBox)
import Model.Constants
import Model.Move

data GhostType = Blinky | Pinky | Inky | Clyde deriving Eq

data Ghost = Ghost {
  ghostType :: GhostType,
  position :: Position,
  direction :: Direction,
  scatter   :: Position,
  buffer :: [InputBuffer]
} deriving (Eq)

instance Moveable Ghost where
  pos (Ghost _ p _ _ _) = p
  buffer (Ghost _ _ _ _ b) = b
  dir (Ghost _ _ d _ _) = d
  move (Ghost t (x, y) _ sp b) d s
    | d == U = Ghost t (x - s, y) d sp b
    | d == D = Ghost t (x + s, y) d sp b
    | d == L = Ghost t (x, y - s) d sp b
    | d == R = Ghost t (x, y + s) d sp b
  

instance Collidable Ghost where
  collisions (Ghost {}) = ["wall", "player"]
  name (Ghost {}) = "ghost"
  hitBox (Ghost _ p@(x, y) _ _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]