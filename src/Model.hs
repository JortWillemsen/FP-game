module Model where
  
import Player ( Player(PuckMan), Toggled (Released), inputBufferWASD )
import Move ()
import Ghost (Ghost (Blinky))

interval :: Float
interval = 0.033

data IsPaused = Play | Pause 
                deriving (Show, Eq)

-- Pauses or unpauses game 
pauseGame :: IsPaused -> IsPaused -- HIER OF CONTROLLER?
pauseGame p | p == Pause = Play
            | otherwise  = Pause

data GameState = GameState {
                   isPaused   :: IsPaused
                 , ticks      :: Float
                 , player     :: Player
                 , blinky     :: Ghost
                 }

initialState :: GameState
initialState = GameState Play 0 (PuckMan (100, 100) inputBufferWASD) (Blinky (0, 0))
