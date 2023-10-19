module Controller where

import Ghost (moveAlgorithm, Ghost (Blinky))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Maze
import Move (Association (..), Position, down, getMove, left, right, up, Move)
import Player (InputBuffer, Player (PuckMan), Toggled (Depressed, Released), resetInputBuffer, Direction (..))
import System.Random
import World

-- | Handle one iteration of the game
step :: Float -> WorldState -> IO WorldState
step secs ws@WorldState {gameState = state}
  | isPaused state == Pause =
      return $
        ws
          { gameState =
              state
                { ticks = ticks state + secs
                }
          }
  | otherwise =
      return $
        ws
          { gameState =
              state
                { player = makePlayerMove (player state) (maze state),
                  blinky = moveAlgorithm (blinky state) (player state),
                  ticks = ticks state + secs
                }
          }
  where
    inputBuffer (PuckMan _ ib _) = [y | (_, y, _) <- ib]

-- If input is received, return changed game state
input :: Event -> WorldState -> IO WorldState
input e ws@WorldState {gameState = state} = return ws {gameState = (inputKey e state)}

-- Check if a key is pressed down change state, otherwise leave state as it was
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) t _ _) state
  | c == 'p' && t == Down = state {isPaused = pauseGame (isPaused state)}
  | c == 'p' && t == Up = state
  | otherwise = state {player = updateInputBuffer c (player state)}
inputKey _ state = state


makeMove :: Move -> Maze -> Position
makeMove (pos, potentialPos) m | moveAllowed potentialPos m = potentialPos
                               | otherwise = pos
    where
        moveAllowed :: Position -> Maze -> Bool
        moveAllowed _ [] = True
        moveAllowed pos' (t:ts) = case t of
                                    (Wall wPos _) | hitbox pos' `intersect` hitbox wPos -> False
                                                  | otherwise -> moveAllowed pos' ts
                                    _ -> moveAllowed pos' ts
            where
                hitbox :: Position -> [Position]
                hitbox p@(x, y) = [p, (x, y+tileSize-0.1), (x+tileSize-0.1, y+tileSize-0.1), (x+tileSize-0.1, y)]

                intersect :: [Position] -> [Position] -> Bool
                intersect [bL, tL, tR, bR] s = inSquare bL s || inSquare tL s || inSquare tR s || inSquare bR s
                    where
                        inSquare :: Position -> [Position] -> Bool
                        inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY


-- when a key is pressed, move player based on which key is pressed
makePlayerMove :: Player -> Maze -> Player
makePlayerMove (PuckMan pos ibs d) m = move ibs pos
  where
    -- applies a move to a positionS
    move :: [InputBuffer] -> Position -> Player
    move ((_, t, a) : ibs') pos' | t == Depressed = if makeMove (pos', getMove a pos') m /= getMove a pos' 
                                                      then PuckMan (makeMove (pos', getMove (getLastMove d) pos') m) ibs d
                                                      else PuckMan (getMove a pos') ibs (newDirection a)
                                 | otherwise = move ibs' pos' 
    move [] pos' = PuckMan (makeMove (pos', getMove (getLastMove d) pos') m) ibs d

    newDirection GoRight = R 
    newDirection GoLeft = L 
    newDirection GoUp = U 
    newDirection GoDown = D

    getLastMove R = GoRight 
    getLastMove L = GoLeft 
    getLastMove U = GoUp 
    getLastMove D = GoDown


-- Updates the input buffer of a player when a key is pressed
updateInputBuffer :: Char -> Player -> Player
updateInputBuffer c (PuckMan pos ibs d) = PuckMan pos (updateInputBuffer' c ibs) d
  where
    -- Updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer' :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer' c [] = []
    updateInputBuffer' c (ib@(k, t, a) : ibs)
      | c == k = (k, Depressed, a) : updateInputBuffer' c ibs
      | otherwise = (k, Released, a) : updateInputBuffer' c ibs
