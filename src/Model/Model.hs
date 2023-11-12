module Model.Model where

import Model.Ghost (Ghost(Ghost), GhostType (Blinky, Pinky, Inky, Clyde), Wellbeing (Spawning, Frightened), newWellbeing, spawn)
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn, ScatterSpawn), pos)
import Model.Player (Player (Player, spawnPoint), PlayerType (PuckMan), inputBufferWASD)
import Model.Score
import Model.Move (Direction(L, U, D, R), Toggled (Released), Moveable (moveTo))
import System.Random (StdGen, mkStdGen)
import Model.Spawning (randomPlayerSpawn, randomSpawns)
import Model.Constants (scatterTime, initiateLives)
import qualified Model.Player as Player
import qualified Model.Player as Ghost

type Time = Float
type Lives = Int

type Level = Int 

data Screen = Show | Hide deriving (Eq)

toggleScreen :: Screen -> Screen
toggleScreen Show = Hide
toggleScreen Hide = Show

data GameState = GameState {
                    maze        :: Maze
                  , lives       :: Lives
                  , score       :: Score
                  , time        :: Time
                  , ticks       :: Float
                  , player      :: Player
                  , blinky      :: Ghost
                  , pinky       :: Ghost
                  , inky        :: Ghost
                  , clyde       :: Ghost
                  , level       :: Level
                  , screenState :: ScreenState
                  , generator   :: StdGen
                } 

data ScreenState = ScreenState 
  { 
    highscoreToggle :: Screen, 
    menuToggle :: Screen, 
    pauseToggle :: Screen 
  }  

-- | Creates a new game state before the 1st tick start
basicState :: [String] -> Level -> Int -> GameState
basicState level l r = 
  GameState 
    maze  
    initiateLives 
    0 
    0 
    0 
    (Player PuckMan playerSpawn inputBufferWASD L playerSpawn)  
    (Ghost Blinky (head ghostSpawns) (head ghostSpawns) L (head scatterSpawns) (Spawning 0))
    (Ghost Pinky (ghostSpawns!!1) (ghostSpawns!!1) R (scatterSpawns!!1) (Spawning 5)) 
    (Ghost Inky (ghostSpawns!!2) (ghostSpawns!!2) L (scatterSpawns!!2) (Spawning 10)) 
    (Ghost Clyde (ghostSpawns!!3) (ghostSpawns!!3) R (scatterSpawns!!3) (Spawning 20))
    l 
    (ScreenState Hide Hide Hide)
    random 
    where
      maze = loadMaze level
      (playerSpawn, gen) = randomPlayerSpawn random maze
      (ghostSpawns, gen') = randomSpawns gen GhostSpawn [1, 2, 3, 4] maze
      (scatterSpawns, _) = randomSpawns gen' ScatterSpawn [1, 2, 3, 4] maze
      random = mkStdGen r

-- | Resets all entities to their spawn position
deathState :: GameState -> GameState
deathState gs = gs {
  blinky = newWellbeing (Spawning 0) $ moveTo (blinky gs) (spawn $ blinky gs),
  pinky = newWellbeing (Spawning 5) $ moveTo (pinky gs) (spawn $ pinky gs),
  inky = newWellbeing (Spawning 10) $ moveTo (inky gs) (spawn $ inky gs),
  clyde = newWellbeing (Spawning 20) $ moveTo (clyde gs) (spawn $ clyde gs),
  player = moveTo (player gs) (spawnPoint $ player gs)
}
