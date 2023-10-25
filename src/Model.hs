module Model where
  

import Move
import Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Player
import Score
import Ghost

type Time = Float

interval :: Time
interval = 0.033

data IsPaused = Play | Pause 
                deriving (Show, Eq)

type Lives = Int 

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
                  , cooldown   :: Int 
                }

-- Takes level for first time maze generation.
initialState :: [String] -> GameState
initialState level = GameState maze Play initiateLives 0 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L) 0 where
  maze = loadMaze level
  playerSpawn = Maze.pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = Maze.pos $ head $ getSpawns GhostSpawn maze

