module Controller.Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Collidable (collides)
import Model.Constants (frightenedTime, normalTime, scatterTime, spawnTime, tileSize)
import Model.Ghost (Ghost (Ghost, ghostType, spawnPoint, wellbeing), GhostType (Blinky, Clyde, Inky, Pinky), Wellbeing (Frightened, Normal, Respawning, Scattered, Spawning), getTime, newWellbeing, sPos, spawn, translateGhost, makeFrightened)
import Model.Maze (Maze, Tile (Floor, Wall), getCollectible, getEnergizers, Collectable (Dot, Energizer), floors, collectable)
import Model.Model
    ( deathState,
      GameState(lives, screenState, level, ticks, time, pinky, inky,
                score, blinky, clyde, player, generator, maze),
      ScreenState(pauseToggle),
      Time )
import Model.Move (Move, Moveable (dir, move, pos), Position, Toggled (Depressed, Released), down, left, manhattan, right, up)
import Model.Player
import Model.Score (updateHighScores, updateScore)
import Model.Collidable
import System.Random
import View.File (saveHighScores)
import View.World

-- | Handle one iteration of the game
step :: Float -> WorldState -> IO WorldState
step interval ws@WorldState {gameState = state}
  -- Check if we need to change to another state
  | pauseToggle (screenState state) == Depressed = return ws
  | gameOver state = handleGameOver ws
  | nextLevel (maze state) = createWorldState (level state + 1)
  -- Otherwise keep running current state
  | otherwise =
      return $
        ws
          { gameState =
              handleEffects
                state
                  { 
                    -- Translate the player
                    player = translatePlayer (player state) (maze state),
                    
                    -- Update ghosts behaviour, wellbeing and position 
                    blinky = updateGhost (blinky state) interval state,
                    pinky = updateGhost (pinky state) interval state,
                    inky = updateGhost (inky state) interval state,
                    clyde = updateGhost (clyde state) interval state,
                    
                    -- Iterate 1 game tick
                    ticks = ticks state + 1,
                    time = time state + interval,

                    -- Generate a new random value every tick
                    generator = snd $ (random :: StdGen -> (Int, StdGen)) (generator state)
                  }
          }

-- | After changing the state we need to check for some effects like:
--   Collision with entities - Has the player collided with a ghost or a collectible
handleEffects :: GameState -> GameState
handleEffects gs = 
  let ghost = collidesReturn (player gs) [blinky gs, pinky gs, inky gs, clyde gs]
      collectible = collidesReturn (player gs) (floors $ maze gs) in
        -- If ghost is just, we have collided with it 
        case ghost of
          Nothing -> 
            -- If collectible is just, we have collided with it
            case collectible of
              Nothing -> gs
              Just x -> handleCollectibleCollision gs x
          Just x -> handleGhostCollision gs x

-- | Updates the game state based on collision with a ghost
handleGhostCollision :: GameState -> Ghost -> GameState
handleGhostCollision gs g = case wellbeing g of
  -- If ghost is respawning we don't want any effect
  Respawning -> gs
  -- If ghost is frightened we just pick the right ghost and make it: Respawning
  (Frightened _) -> case ghostType g of
    Blinky -> gs {blinky = newWellbeing Respawning g}
    Pinky -> gs {pinky = newWellbeing Respawning g}
    Inky -> gs {inky = newWellbeing Respawning g}
    Clyde -> gs {clyde = newWellbeing Respawning g}
  -- In any other case, the ghost eats the player
  _ -> deathState gs {lives = lives gs - 1}

-- | Updates the game state based on collision with a collectible
handleCollectibleCollision :: GameState -> Tile -> GameState
handleCollectibleCollision gs t = case t of
  (Wall {}) -> gs
  -- If collectible is dot, we need to update the score and remove it from the maze
  (Floor _ _ (Just Dot) _) -> gs
    { 
      maze = snd $ updateScore t (maze gs) (score gs),
      score = fst $ updateScore t (maze gs) (score gs)
    }
  -- If collectible is energizer, we need to make every ghost frightened
  (Floor _ _ (Just Energizer) _) -> gs 
    { 
      maze = snd $ updateScore t (maze gs) (score gs),
      score = fst $ updateScore t (maze gs) (score gs),
      blinky = makeFrightened (blinky gs),
      pinky = makeFrightened (pinky gs),
      inky = makeFrightened (inky gs),
      clyde = makeFrightened (clyde gs)
    }

handleGameOver :: WorldState -> IO WorldState
handleGameOver ws = do
  saveHighScores (playerType $ player (gameState ws), score (gameState ws)) (highScores ws)
  createWorldState 1

updateGhost :: Ghost -> Time -> GameState -> Ghost
updateGhost ghost@(Ghost t p sp d scp w ib) interval state = translateGhost (Ghost t p sp d scp (updateWellbeing ghost interval) ib) (generator state) (ghostTarget ghost) (maze state)
  where
    ghostTarget :: Ghost -> Model.Move.Position
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