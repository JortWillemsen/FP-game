module Controller.Controller where

import Model.Ghost (Ghost)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Maze
import Model.Model
import Model.Move (Move, Position, down, left, right, up, Moveable (move), translatePlayer, translateGhost, Toggled (Released, Depressed))
import Model.Player
import Model.Score (updateScore, updateHighScores)
import View.World
import View.File (saveHighScores)

-- | Handle one iteration of the game
step :: Float -> WorldState -> IO WorldState
step interval ws@WorldState {gameState = state}
  | pauseToggle (screenState state) == Depressed = return ws
  | gameOver state = handleGameOver (gameState ws)
  | nextLevel (maze state) = createWorldState (level state + 1)
  | otherwise =
      return $
        ws
          { gameState =
              state
                { player = translatePlayer (player state) (maze state),
                  score = updatedScore,
                  maze = updatedMaze,
                  blinky = translateGhost (blinky state) (position $ player state) (maze state),
                  ticks = ticks state + 1,
                  time = time state + interval
                }
          }
  where
    (updatedScore, updatedMaze) = updateScore (position (player state)) (maze state) (score state)

handleGameOver :: GameState -> IO WorldState
handleGameOver state = do 
  saveHighScores (show $ playerType $ player state, score state) 
  createWorldState 1  

gameOver :: GameState -> Bool
gameOver state = lives state == 0 

nextLevel :: Maze -> Bool 
nextLevel m = all (== Nothing) [getCollectible m p | (Floor p _ _) <- m]