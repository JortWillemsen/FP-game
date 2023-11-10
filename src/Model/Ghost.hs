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

-- | Every ghost has a wellbeing that determines their target position 
--   and if they can be eaten by the player
data Wellbeing  = Scattered Time 
                | Normal Time 
                | Frightened Time
                | Respawning 
                | Spawning Time deriving Eq

-- | Finds the time remaining in this wellbeing
getTime :: Wellbeing -> Time
getTime (Normal t) = t
getTime (Scattered t) = t
getTime (Frightened t) = t
getTime (Spawning t) = t
getTime Respawning = 0

data Ghost = Ghost {
  ghostType :: GhostType, -- type of ghost
  position :: Position,   -- current position
  spawnPoint :: Position, -- spawn point
  direction :: Direction, -- direction facing
  scatter   :: Position,  -- position to target in scatter wellbeing
  wellbeing :: Wellbeing, -- wellbeing of the ghost
  buffer :: [InputBuffer] -- Input buffer if ever needed
} deriving (Eq)

instance Moveable Ghost where
  pos :: Ghost -> Position
  pos (Ghost _ p _ _ _ _ _) = p
  buffer :: Ghost -> [InputBuffer]
  buffer (Ghost _ _ _ _ _ _ b) = b
  dir :: Ghost -> Direction
  dir (Ghost _ _ _ d _ _ _) = d
  moveTo :: Ghost -> Position -> Ghost
  moveTo (Ghost t p sp d scp w b) new = Ghost t new sp d scp w b
  move :: Ghost -> Direction -> Speed -> Ghost
  move (Ghost t (x, y) sp _ scp w b) d s
    | d == U = Ghost t (x - s, y) sp d scp w b
    | d == D = Ghost t (x + s, y) sp d scp w b
    | d == L = Ghost t (x, y - s) sp d scp w b
    | d == R = Ghost t (x, y + s) sp d scp w b
  
-- | We need this function since it won't work when both whe player and ghost records are imported for some reason
spawn :: Ghost -> Position
spawn = spawnPoint

instance Collidable Ghost where
  collisions :: Ghost -> [String]
  collisions (Ghost {}) = ["wall", "player"]
  name :: Ghost -> String
  name (Ghost {}) = "ghost"
  hitBox :: Ghost -> HitBox
  hitBox (Ghost _ p@(x, y) _ _ _ _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

-- | Inserts a new wellbeing in the ghost
newWellbeing :: Wellbeing -> Ghost -> Ghost
newWellbeing newW (Ghost t p sp d scp w b) = Ghost t p sp d scp newW b

-- | Makes the ghost frightened if it is not spawning or respawning
makeFrightened :: Ghost -> Ghost
makeFrightened g = case wellbeing g of
  Respawning -> g
  (Spawning _) -> g
  _ -> newWellbeing (Frightened frightenedTime) g

-- | Moves the ghost based on collisions and where it wants to move
translateGhost :: (Collidable a, RandomGen g) => Ghost -> g -> Position -> [a] -> Ghost
translateGhost g gen p cs = case wellbeing g of
  -- If the ghost is frightened we want to select a random move based on the moves it can do
  (Frightened _) -> if (length (possibleMoves movesPerDir) < 1)
    -- If we have no available moves, we want to turn back
    then move g (inverse $ dir g) 0
    else fst $ fst $ randomElementFromList (possibleMoves movesPerDir) gen
  
  -- otherwise we want to select the move most likely to get us to our target
  _ -> if length sortedMoves < 1
    then move g (inverse $ dir g) 0
    else fst $ head sortedMoves
  where  
    -- | Sorts the moves based on the 
    sortedMoves :: [(Ghost, Float)]
    sortedMoves = sortBy (compare `on` snd) $ possibleMoves movesPerDir

    -- | Finds possible moves and assignes a value based on how likely we are to find the target
    possibleMoves :: [Maybe Ghost] -> [(Ghost, Float)]
    possibleMoves = foldr f [] where
      f (Just x) r = (x, pythagoras (pos x) p) : r
      f Nothing r = r
    
    -- | Tries to move in every direction we can
    movesPerDir :: [Maybe Ghost]
    movesPerDir = case wellbeing g of
      (Spawning _)-> []
      _ -> case dir g of
        L -> [tryMove g D cs, tryMove g L cs, tryMove g U cs]
        R -> [tryMove g D cs, tryMove g R cs, tryMove g U cs]
        U -> [tryMove g L cs, tryMove g U cs, tryMove g R cs]
        D -> [tryMove g L cs, tryMove g D cs, tryMove g R cs]