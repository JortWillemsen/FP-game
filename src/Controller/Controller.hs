module Controller.Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Constants (tileSize)
import Model.Ghost (Ghost)
import Model.Maze
import Model.Model
import Model.Move (Move, Moveable (dir, move), Position, down, left, right, translateGhost, translatePlayer, up)
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
                  pinky = translateGhost (pinky state) pinkyTarget (maze state),
                  inky = translateGhost (inky state) inkyTarget (maze state),
                  clyde = translateGhost (clyde state) clydeTarget (maze state),
                  ticks = ticks state + 1,
                  time = time state + interval
                }
          }
  where
    updatedScore = updateScore (position (player state)) (maze state) (score state)
    pinkyTarget = position $ move (player state) (dir $ player state) (tileSize * 2)
    inkyTarget = position $ move (player state) (dir $ player state) (tileSize * 2)
    clydeTarget = position $ move (player state) (dir $ player state) (tileSize * 2)

gameOver :: GameState -> Bool
gameOver state = lives state == 0

nextLevel :: Maze -> Bool
nextLevel m = all (== Nothing) [getCollectible m p | (Floor _ p _ _) <- m]