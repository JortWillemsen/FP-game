module Controller.Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Constants (tileSize, scatterTime, normalTime)
import Model.Ghost (Ghost (Ghost, wellbeing, ghostType, spawnPoint), sPos, Wellbeing (Normal, Scattered, Frightened, Spawning), GhostType (Blinky, Pinky, Inky, Clyde), getTime, translateGhost)
import Model.Maze (Maze, Tile (Floor), getCollectible)
import Model.Model
import Model.Move (Move, Position, down, left, right, up, Moveable (move, dir, pos), Toggled (Released, Depressed), manhattan)
import Model.Player
import Model.Score (updateScore, updateHighScores)
import View.World
import View.File (saveHighScores)
import System.Random

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
                  blinky =  updateGhost (blinky state) interval state,
                  pinky = updateGhost (pinky state) interval state,
                  inky = updateGhost (inky state) interval state,
                  clyde = updateGhost (clyde state) interval state,
                  ticks = ticks state + 1,
                  time = time state + interval,
                  generator = snd $ (random :: StdGen -> (Int, StdGen)) (generator state)
                }
          }
  where
    (updatedScore, updatedMaze) = updateScore (position (player state)) (maze state) (score state)

handleGameOver :: GameState -> IO WorldState
handleGameOver state = do 
  saveHighScores (show $ playerType $ player state, score state) 
  createWorldState 1  

updateGhost :: Ghost -> Time -> GameState -> Ghost
updateGhost ghost@(Ghost t p sp d scp w ib) interval state = translateGhost (Ghost t p sp d scp (updateWellbeing w interval) ib) (generator state) (ghostTarget ghost) (maze state)  where
  ghostTarget :: Ghost -> Position
  ghostTarget g = case wellbeing g of
    (Spawning _) -> spawnPoint g
    (Scattered _) -> sPos g
    otherwise -> case ghostType g of
      Blinky -> blinkyTarget
      Pinky -> pinkyTarget
      Inky -> inkyTarget
      Clyde -> clydeTarget
  blinkyTarget = position $ player state
  pinkyTarget = position $ move (player state) (dir $ player state) (tileSize * 2)
  inkyTarget = 
    let (x, y) = position $ move (player state) (dir $ player state) (tileSize * 2) 
        distance = manhattan (pos $ blinky state) (position $ player state) in
          (x + distance, y + distance)
  clydeTarget =
    if manhattan (pos $ clyde state) (pos $ player state) < 5
      then sPos (clyde state)
      else position $ player state

updateWellbeing :: Wellbeing -> Time -> Wellbeing
updateWellbeing w i = if (getTime w - i) <= 0
  then case w of
    (Normal _) -> Scattered scatterTime
    (Scattered _) -> Normal normalTime
    (Frightened _) -> Scattered scatterTime
    (Spawning _) -> Normal normalTime
  else case w of
    (Normal t) -> Normal (t-i)
    (Scattered t) -> Scattered (t-i)
    (Frightened t) -> Frightened (t-i)
    (Spawning t) -> Spawning (t-i)

gameOver :: GameState -> Bool
gameOver state = lives state == 0

nextLevel :: Maze -> Bool
nextLevel m = all (== Nothing) [getCollectible m p | (Floor _ p _ _) <- m]