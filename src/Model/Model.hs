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
                  , level      :: Level
                  , menuState  :: MenuState
                } 

data MenuState = MenuState { levels :: [Int], toggled :: Bool } -- maybe Toggled type 

-- Takes level for first time maze generation.
nextState :: [String] -> Level -> GameState
nextState level l = GameState maze Play initiateLives 0 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L inputBufferWASD) l (MenuState [1] False) where
  maze = loadMaze level
  playerSpawn = pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = pos $ head $ getSpawns GhostSpawn maze
