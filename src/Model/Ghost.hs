{-# LANGUAGE InstanceSigs #-}
module Model.Ghost where

import Model.Collidable (Collidable (hitBox, collisions, name), HitBox)
import Model.Constants
import Model.Move
import Data.List (sortBy)
import Data.Function (on)
import View.Random (randomElementFromList)
import System.Random (RandomGen)

type Time = Float

data GhostType = Blinky | Pinky | Inky | Clyde deriving Eq
data Wellbeing = Scattered Time | Normal Time | Frightened Time deriving Eq

getTime :: Wellbeing -> Time
getTime (Normal t) = t
getTime (Scattered t) = t
getTime (Frightened t) = t

data Ghost = Ghost {
  ghostType :: GhostType,
  position :: Position,
  direction :: Direction,
  scatter   :: Position,
  wellbeing :: Wellbeing,
  buffer :: [InputBuffer]
} deriving (Eq)

instance Moveable Ghost where
  pos (Ghost _ p _ _ _ _) = p
  buffer (Ghost _ _ _ _ _ b) = b
  dir (Ghost _ _ d _ _ _) = d
  move :: Ghost -> Direction -> Speed -> Ghost
  move (Ghost t (x, y) _ sp w b) d s
    | d == U = Ghost t (x - s, y) d sp w b
    | d == D = Ghost t (x + s, y) d sp w b
    | d == L = Ghost t (x, y - s) d sp w b
    | d == R = Ghost t (x, y + s) d sp w b
  

instance Collidable Ghost where
  collisions (Ghost {}) = ["wall", "player"]
  name (Ghost {}) = "ghost"
  hitBox (Ghost _ p@(x, y) _ _ _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

sPos :: Ghost -> Position
sPos (Ghost _ _ _ s _ _) = s

translateGhost :: (Collidable a, RandomGen g) => Ghost -> g -> Position -> [a] -> Ghost
translateGhost g gen p cs = case wellbeing g of
  (Frightened _) -> fst $ fst $ randomElementFromList (possibleMoves movesPerDir) gen
  otherwise -> if length sortedMoves < 1
    then move g (inverse $ dir g) speed
    else fst $ head sortedMoves
  where  
    sortedMoves = sortBy (compare `on` snd) $ possibleMoves movesPerDir
    possibleMoves moves = foldr f [] moves
    f (Just x) r = (x, manhattan (pos x) p) : r
    f Nothing r = r
    movesPerDir = case dir g of
      L -> [tryMove g D cs, tryMove g L cs,  tryMove g U cs]
      R -> [tryMove g D cs, tryMove g R cs, tryMove g U cs]
      U -> [tryMove g L cs, tryMove g U cs, tryMove g R cs]
      D -> [tryMove g L cs, tryMove g D cs, tryMove g R cs]