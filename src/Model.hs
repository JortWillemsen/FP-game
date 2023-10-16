module Model where
  
import Player ( Player(PuckMan), Toggled (Released), inputBufferWASD )
import Move ()
import Ghost (Ghost (Blinky))
import Maze (Maze, basicMaze, loadMaze)

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
initialState level = GameState (loadMaze level) Play 0 (PuckMan (100, 100) inputBufferWASD) (Blinky (0, 0))
