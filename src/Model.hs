module Model where
  
import Player ( Player(PuckMan), Toggled (Released), inputBufferWASD )
import Move
import Ghost (Ghost (Blinky))
import Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)

interval :: Float
interval = 0.033

data IsPaused = Play | Pause 
                deriving (Show, Eq)

-- Pauses or unpauses game 
pauseGame :: IsPaused -> IsPaused -- HIER OF CONTROLLER?
pauseGame p | p == Pause = Play
            | otherwise  = Pause
data GameState = GameState {
                    maze         :: Maze
                  , isPaused   :: IsPaused
                  , ticks      :: Float
                  , player     :: Player
                  , blinky     :: Ghost
                }

-- Takes level for first time maze generation.
initialState :: [String] -> GameState
initialState level = GameState maze Play 0 (PuckMan playerSpawn inputBufferWASD) (Blinky ghostSpawn) where
  maze = loadMaze level
  playerSpawn = pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = pos $ head $ getSpawns GhostSpawn maze

