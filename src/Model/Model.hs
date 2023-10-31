module Model.Model where
  

import Model.Ghost
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Model.Player
import Model.Score
import Model.Move (Direction(L, U, D, R))
import System.Random (StdGen, mkStdGen)
import Model.Spawning (randomPlayerSpawn, randomGhostSpawns, randomScatterSpawns)
import Model.Constants (scatterTime)

interval :: Time
interval = 0.033

data IsPaused = Play | Pause
                deriving (Show, Eq)

type Lives = Int
type Level = Int

initiateLives :: Lives
initiateLives = 3

-- Pauses or unpauses game 
pauseGame :: IsPaused -> IsPaused -- HIER OF CONTROLLER?
pauseGame p | p == Pause = Play
            | otherwise  = Pause

data GameState = GameState {
                    maze       :: Maze
                  , isPaused   :: IsPaused
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
                  , menuState  :: MenuState
                  , generator     :: StdGen
                } 

data MenuState = MenuState { levels :: [Int], toggled :: Bool } -- maybe Toggled type 

-- Takes level for first time maze generation.
nextState :: [String] -> Level -> Int -> GameState
nextState level l r = 
  GameState 
    maze 
    Play 
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
    (MenuState [1] False) 
    random 
    where
      maze = loadMaze level
      (playerSpawn, gen) = randomPlayerSpawn random maze
      (ghostSpawns, gen') = randomGhostSpawns gen [1, 2, 3, 4] maze
      (scatterSpawns, _) = randomScatterSpawns gen' [1, 2, 3, 4] maze
      random = mkStdGen r
