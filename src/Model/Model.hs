module Model.Model where
  

import Model.Ghost
import Model.Maze (Maze, loadMaze, getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn), pos)
import Model.Player
import Model.Score
import Model.Move (Direction(L), Toggled (Released))

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
                  , level      :: Level
                  , screenState :: ScreenState
                } 

data ScreenState = ScreenState { highscoreToggle :: Toggled
                               , menuToggle :: Toggled
                               , pauseToggle :: Toggled } -- maybe Toggled type 

-- Takes level for first time maze generation.
nextState :: [String] -> Level -> GameState
nextState level l = GameState maze initiateLives 0 0 0 (Player PuckMan playerSpawn inputBufferWASD L) (Ghost Blinky ghostSpawn L inputBufferWASD) l (ScreenState Released Released Released) where
  maze = loadMaze level
  playerSpawn = pos $ head $ getSpawns PlayerSpawn maze
  ghostSpawn = pos $ head $ getSpawns GhostSpawn maze
