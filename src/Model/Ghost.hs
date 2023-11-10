{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
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
data Wellbeing  = Scattered Time 
                | Normal Time 
                | Frightened Time
                | Respawning 
                | Spawning Time deriving Eq

getTime :: Wellbeing -> Time
getTime (Normal t) = t
getTime (Scattered t) = t
getTime (Frightened t) = t
getTime (Spawning t) = t
getTime Respawning = 0

data Ghost = Ghost {
  ghostType :: GhostType,
  position :: Position,
  spawnPoint :: Position,
  direction :: Direction,
  scatter   :: Position,
  wellbeing :: Wellbeing,
  buffer :: [InputBuffer]
} deriving (Eq)

instance Moveable Ghost where
  pos (Ghost _ p _ _ _ _ _) = p
  buffer (Ghost _ _ _ _ _ _ b) = b
  dir (Ghost _ _ _ d _ _ _) = d
  moveTo (Ghost t p sp d scp w b) new = Ghost t new sp d scp w b
  move :: Ghost -> Direction -> Speed -> Ghost
  move (Ghost t (x, y) sp _ scp w b) d s
    | d == U = Ghost t (x - s, y) sp d scp w b
    | d == D = Ghost t (x + s, y) sp d scp w b
    | d == L = Ghost t (x, y - s) sp d scp w b
    | d == R = Ghost t (x, y + s) sp d scp w b
  

instance Collidable Ghost where
  collisions (Ghost {}) = ["wall", "player"]
  name (Ghost {}) = "ghost"
  hitBox (Ghost _ p@(x, y) _ _ _ _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

sPos :: Ghost -> Position
sPos (Ghost _ _ _ _ s _ _) = s

spawn :: Ghost -> Position
spawn (Ghost _ _ sp _ _ _ _) = sp

newWellbeing :: Wellbeing -> Ghost -> Ghost
newWellbeing newW (Ghost t p sp d scp w b) = Ghost t p sp d scp newW b

makeFrightened :: Ghost -> Ghost
makeFrightened g = case wellbeing g of
  Respawning -> g
  (Spawning _) -> g
  _ -> newWellbeing (Frightened frightenedTime) g

translateGhost :: (Collidable a, RandomGen g) => Ghost -> g -> Position -> [a] -> Ghost
translateGhost g gen p cs = case wellbeing g of
  (Frightened _) -> if (length (possibleMoves movesPerDir) < 1)
    then move g (inverse $ dir g) 0
    else fst $ fst $ randomElementFromList (possibleMoves movesPerDir) gen
  otherwise -> if length sortedMoves < 1
    then move g (inverse $ dir g) 0
    else fst $ head sortedMoves
  where  
    sortedMoves = sortBy (compare `on` snd) $ possibleMoves movesPerDir
    possibleMoves = foldr f []
    f (Just x) r = (x, manhattan (pos x) p) : r
    f Nothing r = r
    movesPerDir = case wellbeing g of
      (Spawning _)-> []
      _ -> case dir g of
        L -> [tryMove g D cs, tryMove g L cs,  tryMove g U cs]
        R -> [tryMove g D cs, tryMove g R cs, tryMove g U cs]
        U -> [tryMove g L cs, tryMove g U cs, tryMove g R cs]
        D -> [tryMove g L cs, tryMove g D cs, tryMove g R cs]