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
-- initialState :: [String] -> GameState
-- initialState level = GameState maze Play initiateLives 0 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L) 1 (MenuState [1] True) where
--   maze = loadMaze level
--   playerSpawn = Maze.pos $ head $ getSpawns PlayerSpawn maze
--   ghostSpawn = Maze.pos $ head $ getSpawns GhostSpawn maze
nextState :: [String] -> Level -> GameState
nextState level l = GameState maze Play initiateLives 0 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L) l (MenuState [1] False) where
  maze = loadMaze level
  playerSpawn = Maze.pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = Maze.pos $ head $ getSpawns GhostSpawn maze
