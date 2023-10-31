module Model.Model where
  

import Model.Ghost (Ghost(Ghost), GhostType (Blinky, Pinky, Inky, Clyde), Wellbeing (Spawning, Frightened), newWellbeing, spawn)
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Model.Player (Player (Player, spawnPoint), PlayerType (PuckMan), inputBufferWASD)
import Model.Score
import Model.Move (Direction(L, U, D, R), Toggled (Released), Moveable (moveTo))
import System.Random (StdGen, mkStdGen)
import Model.Spawning (randomPlayerSpawn, randomGhostSpawns, randomScatterSpawns)
import Model.Constants (scatterTime)
import qualified Model.Player as Player
import qualified Model.Player as Ghost

type Time = Float

interval :: Time
interval = 0.033

type Lives = Int
type Level = Int

initiateLives :: Lives
initiateLives = 3

data GameState = GameState {
                    maze       :: Maze
                  , lives      :: Lives
                  , score      :: Score
                  , time       :: Time
                  , ticks      :: Float
                  , player     :: Player
                  , blinky     :: Ghost
                  , pinky      :: Ghost
                  , inky       :: Ghost
                  , clyde      :: Ghost
                  , level      :: Level
                  , screenState :: ScreenState
                  , generator     :: StdGen
                } 

data ScreenState = ScreenState { highscoreToggle :: Toggled
                               , menuToggle :: Toggled
                               , pauseToggle :: Toggled } -- maybe Toggled type 

-- Takes level for first time maze generation.
nextState :: [String] -> Level -> Int -> GameState
nextState level l r = 
  GameState 
    maze  
    initiateLives 
    0 
    0 
    0 
    (Player PuckMan playerSpawn inputBufferWASD L playerSpawn)  
    (Ghost Blinky (ghostSpawns!!0) (ghostSpawns!!0) L (scatterSpawns!!0) (Spawning 0) inputBufferWASD)
    (Ghost Pinky (ghostSpawns!!1) (ghostSpawns!!1) R (scatterSpawns!!1) (Spawning 5) inputBufferWASD) 
    (Ghost Inky (ghostSpawns!!2) (ghostSpawns!!2) L (scatterSpawns!!2) (Spawning 10) inputBufferWASD) 
    (Ghost Clyde (ghostSpawns!!3) (ghostSpawns!!3) R (scatterSpawns!!3) (Spawning 20) inputBufferWASD)
    l 
    (ScreenState Released Released Released) 
    random 
    where
      maze = loadMaze level
      (playerSpawn, gen) = randomPlayerSpawn random maze
      (ghostSpawns, gen') = randomGhostSpawns gen [1, 2, 3, 4] maze
      (scatterSpawns, _) = randomScatterSpawns gen' [1, 2, 3, 4] maze
      random = mkStdGen r

deathState :: GameState -> GameState
deathState gs = gs {
  blinky = newWellbeing (Spawning 0) $ moveTo (blinky gs) (spawn $ blinky gs),
  pinky = newWellbeing (Spawning 5) $ moveTo (pinky gs) (spawn $ pinky gs),
  inky = newWellbeing (Spawning 10) $ moveTo (inky gs) (spawn $ inky gs),
  clyde = newWellbeing (Spawning 20) $ moveTo (clyde gs) (spawn $ clyde gs),
  player = moveTo (player gs) (spawnPoint $ player gs)
}
