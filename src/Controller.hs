module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Maze
import Model
import Move (Association (..), Move, Position, directionToMove, down, getMove, left, moveToDirection, right, up)
import Player
import System.Random
import World
import Score (updateScore, updateHighScores)
import Ghost (collidesWithPlayer)

-- | Handle one iteration of the game
step :: Float -> WorldState -> IO WorldState -- gebruik maken van een do-block
step interval ws@WorldState {gameState = state}
  | isPaused state == Pause =
      return $
        ws
          { gameState =
              state
                { ticks = ticks state + 1,
                  time = time state + interval
                }
          }
  | gameOver state = initialWorldState -- temporary
  | otherwise =
      return $
        ws
          { gameState =
              state
                { player = makePlayerMove (player state) (maze state),
                  score = fst updatedScore,
                  maze = snd updatedScore,
                  cooldown = if changeLives /= lives state then 30 else setCoolDown (cooldown state),
                  lives = changeLives,
                  -- blinky = moveAlgorithm (blinky state) (player state) (maze state),
                  ticks = ticks state + 1,
                  time = time state + interval
                }
          }
  where
    setCoolDown n | n /= 0 = n - 1
                  | otherwise = n
    changeLives = if collidesWithPlayer (player state) (blinky state) && cooldown state == 0
                    then lives state - 1
                    else lives state
    updatedScore = updateScore (position (player state)) (maze state) (score state)
    inputBuffer (Player _ _ ib _) = [y | (_, y, _) <- ib]

-- if input is received, return changed world state
input :: Event -> WorldState -> IO WorldState
input e ws@WorldState {gameState = state} = return ws {gameState = inputKey e state}

gameOver :: GameState -> Bool
gameOver state = lives state == 0 

-- check if a key is pressed down change state, otherwise leave state as it was
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) t _ _) state 
  = case c of 
    'p' | t == Down -> state {isPaused = pauseGame (isPaused state)}
        | t == Up -> state
    _ -> state {player = updateInputBuffer c (player state)}
inputKey _ state = state

makeMove :: Move -> Maze -> Position
makeMove (pos, potentialPos) m
  | moveAllowed potentialPos m = potentialPos -- if move allowed, make move
  | otherwise = pos -- if move not allowed, stay where you are
  where
    moveAllowed :: Position -> Maze -> Bool -- check if move is allowed
    moveAllowed _ [] = True
    moveAllowed pos' (t : ts) = case t of
      (Wall wPos _)
        | hitbox pos' `intersect` hitbox wPos -> False -- if moveable object intersects with a wall, move not allowed
        | otherwise -> moveAllowed pos' ts
      _ -> moveAllowed pos' ts
      where
        hitbox :: Position -> [Position] -- TODO zou dit nog anders kunnen?
        hitbox p@(x, y) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]

        intersect :: [Position] -> [Position] -> Bool
        intersect hitbox s = any (`inSquare` s) hitbox
          where
            inSquare :: Position -> [Position] -> Bool
            inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY

-- when a key is pressed, move player based on which key is pressed
makePlayerMove :: Player -> Maze -> Player
makePlayerMove (Player s pos ibs d) m = move ibs pos
  where
    -- applies a move to a position
    move :: [InputBuffer] -> Position -> Player
    move ((_, t, a) : ibs') pos'
      | t == Depressed =
          if makeMove (pos', getMove a pos') m /= getMove a pos' -- TODO ANDERS
            then Player s (makeMove (pos', getMove (directionToMove d) pos') m) ibs d
            else Player s (getMove a pos') ibs (moveToDirection a)
      | otherwise = move ibs' pos'
    move [] pos' = Player PuckMan (makeMove (pos', getMove (directionToMove d) pos') m) ibs d

-- updates the input buffer of a player when a key is pressed
updateInputBuffer :: Char -> Player -> Player
updateInputBuffer c (Player s pos ibs d) = Player s pos (updateInputBuffer' c ibs) d
  where
    -- updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer' :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer' c [] = []
    updateInputBuffer' c (ib@(k, t, a) : ibs)
      | c == k = (k, Depressed, a) : updateInputBuffer' c ibs
      | otherwise = (k, Released, a) : updateInputBuffer' c ibs
