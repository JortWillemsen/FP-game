module Model where
  
import Player ( Player(PuckMan) )
import Moveable ()

data InfoToShow = ShowNothing
                | ShowPlayer

interval :: Float
interval = 0.033

type IsKeyPressed = Maybe Char

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , ticks :: Float
                 , player :: Player
                 , isKeyPressed :: IsKeyPressed
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 (PuckMan (0, 0)) Nothing 