module Controller.Controller where

import Model.Ghost (Ghost)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Maze
import Model.Model
import Model.Move (Move, Position, down, left, right, up, Moveable (move), translatePlayer, translateGhost)
import Model.Player
import Model.Score (updateScore)
import View.World

-- | Handle one iteration of the game
step :: Float -> WorldState -> IO WorldState
step interval ws@WorldState {gameState = state}
  | isPaused state == Pause = return ws
  | gameOver state = createWorldState 1 -- temporary
  | nextLevel (maze state) = createWorldState (level state + 1)
  | otherwise =
      return $
        ws
          { gameState =
              state
                { player = translatePlayer (player state) (maze state),
                  score = fst updatedScore,
                  maze = snd updatedScore,
                  blinky = translateGhost (blinky state) (position $ player state) (maze state),
                  ticks = ticks state + 1,
                  time = time state + interval
                }
          }
  where
    updatedScore = updateScore (position (player state)) (maze state) (score state)


gameOver :: GameState -> Bool
gameOver state = lives state == 0 

nextLevel :: Maze -> Bool 
nextLevel m = all (== Nothing) [getCollectible m p | (Floor p _ _) <- m]