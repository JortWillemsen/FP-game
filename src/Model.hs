module Model where
  
import Player ( Player(PuckMan), Toggled (Released), inputBufferWASD )
import Move ()

data InfoToShow = ShowNothing
                | ShowPlayer

interval :: Float
interval = 0.033

data IsPaused = Play | Pause 
                deriving (Show, Eq)

data GameState = GameState {
                   infoToShow :: InfoToShow
                 , isPaused   :: IsPaused
                 , ticks      :: Float
                 , player     :: Player
                 }

initialState :: GameState
initialState = GameState ShowNothing Play 0 (PuckMan (0, 0) inputBufferWASD) 
