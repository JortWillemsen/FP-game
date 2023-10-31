module Model.Model where
  

import Model.Ghost (Ghost(Ghost), GhostType (Blinky, Pinky, Inky, Clyde), Wellbeing (Spawning))
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Model.Player
import Model.Score
import Model.Move (Direction(L, U, D, R), Toggled (Released))
import System.Random (StdGen, mkStdGen)
import Model.Spawning (randomPlayerSpawn, randomGhostSpawns, randomScatterSpawns)
import Model.Constants (scatterTime)

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
    (Player PuckMan playerSpawn inputBufferWASD L)  
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
