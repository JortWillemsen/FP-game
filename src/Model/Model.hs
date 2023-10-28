module Model.Model where
  

import Model.Ghost
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Model.Player
import Model.Score
import Model.Move (Direction(L))

type Time = Float

interval :: Time
interval = 0.033

data IsPaused = Play | Pause 
                deriving (Show, Eq)

-- Pauses or unpauses game 
pauseGame :: IsPaused -> IsPaused -- HIER OF CONTROLLER?
pauseGame p | p == Pause = Play
            | otherwise  = Pause
data GameState = GameState {
                    maze       :: Maze
                  , isPaused   :: IsPaused
                  , score      :: Score
                  , time       :: Time
                  , ticks      :: Float
                  , player     :: Player
                  , blinky     :: Ghost
                }

-- Takes level for first time maze generation.
initialState :: [String] -> GameState
initialState level = GameState maze Play ("", 0) 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L inputBufferWASD) where
  maze = loadMaze level
  playerSpawn = pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = pos $ head $ getSpawns GhostSpawn maze

