module Controller.Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Collidable (collides)
import Model.Constants (frightenedTime, normalTime, scatterTime, spawnTime, tileSize)
import Model.Ghost (Ghost (Ghost, ghostType, spawnPoint, wellbeing), GhostType (Blinky, Clyde, Inky, Pinky), Wellbeing (Frightened, Normal, Respawning, Scattered, Spawning), getTime, newWellbeing, sPos, spawn, translateGhost)
import Model.Maze (Maze, Tile (Floor), getCollectible, getEnergizers, Collectable (Dot, Energizer), floors)
import Model.Model
import Model.Move (Move, Moveable (dir, move, pos), Position, Toggled (Depressed, Released), down, left, manhattan, right, up)
import Model.Player
import Model.Score (updateHighScores, updateScore)
import System.Random
import View.File (saveHighScores)
import View.World

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
              handleEffects
                state
                  { player = translatePlayer (player state) (maze state),
                    blinky = updateGhost (blinky state) interval state,
                    pinky = updateGhost (pinky state) interval state,
                    inky = updateGhost (inky state) interval state,
                    clyde = updateGhost (clyde state) interval state,
                    ticks = ticks state + 1,
                    time = time state + interval,
                    generator = snd $ (random :: StdGen -> (Int, StdGen)) (generator state)
                  }
          }

handleEffects :: GameState -> GameState
handleEffects gs
  | collidedGhost = case wb of
      (Frightened _) -> case ghost of
        Nothing -> gs
        (Just x) -> case ghostType x of
          Blinky -> gs {blinky = respawnGhost x}
          Pinky -> gs {pinky = respawnGhost x}
          Inky -> gs {inky = respawnGhost x}
          Clyde -> gs {clyde = respawnGhost x}
      Respawning -> gs
      _ -> deathState gs {lives = (lives gs) - 1}
  | collidedCollectible = case collectible of
      Nothing -> gs
      (Just x@(Floor _ p (Just Energizer) _)) ->
        gs
          { 
            maze = snd $ updateScore x (maze gs) (score gs),
            score = fst $ updateScore x (maze gs) (score gs),
            blinky = makeFrightened (blinky gs),
            pinky = makeFrightened (pinky gs),
            inky = makeFrightened (inky gs),
            clyde = makeFrightened (clyde gs)
          }
      (Just x@(Floor _ p (Just Dot) _)) ->  gs
          { 
            maze = snd $ updateScore x (maze gs) (score gs),
            score = fst $ updateScore x (maze gs) (score gs)
          }
  | otherwise = gs
  where
    makeFrightened :: Ghost -> Ghost
    makeFrightened g = case wellbeing g of
      (Respawning) -> g
      (Spawning _) -> g
      _ -> newWellbeing (Frightened frightenedTime) g
    respawnGhost (Ghost t pos sp d sct _ b) = Ghost t pos sp d sct (Respawning) b
    (collidedCollectible, collectible) = foldr f (False, Nothing) (floors (maze gs))
      where
        f x r =
          if player gs `collides` x
            then (True, Just x)
            else r
    (collidedGhost, wb, ghost) = foldr f (False, Normal 0, Nothing) [blinky gs, pinky gs, inky gs, clyde gs]
      where
        f x r =
          if player gs `collides` x
            then (True, wellbeing x, Just x)
            else r

handleGameOver :: GameState -> IO WorldState
handleGameOver state = do
  -- saveHighScores (show $ playerType $ player state, score state)
  createWorldState 1

updateGhost :: Ghost -> Time -> GameState -> Ghost
updateGhost ghost@(Ghost t p sp d scp w ib) interval state = translateGhost (Ghost t p sp d scp (updateWellbeing ghost interval) ib) (generator state) (ghostTarget ghost) (maze state)
  where
    ghostTarget :: Ghost -> Position
    ghostTarget g = case wellbeing g of
      Respawning -> spawn g
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
          distance = manhattan (pos $ blinky state) (position $ player state)
       in (x + distance, y + distance)
    clydeTarget =
      if manhattan (pos $ clyde state) (pos $ player state) < 5
        then sPos (clyde state)
        else position $ player state

updateWellbeing :: Ghost -> Time -> Wellbeing
updateWellbeing g i =
  if (getTime (wellbeing g) - i) <= 0
    then case wellbeing g of
      (Normal _) -> Scattered scatterTime
      (Scattered _) -> Normal normalTime
      (Frightened _) -> Scattered scatterTime
      (Spawning _) -> Normal normalTime
      Respawning ->
        if (pos g /= spawn g)
          then Respawning
          else Spawning spawnTime
    else case wellbeing g of
      (Normal t) -> Normal (t - i)
      (Scattered t) -> Scattered (t - i)
      (Frightened t) -> Frightened (t - i)
      (Spawning t) -> Spawning (t - i)

gameOver :: GameState -> Bool
gameOver state = lives state == 0

nextLevel :: Maze -> Bool
nextLevel m = all (== Nothing) [getCollectible m p | (Floor _ p _ _) <- m]