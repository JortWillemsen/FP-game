module Controller where

import Ghost (moveAlgorithm)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Maze
import Move (Association (..), Position, down, getMove, left, right, up)
import Player (InputBuffer, Player (PuckMan), Toggled (Depressed, Released), resetInputBuffer)
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
                { player = makeMove (player state) (maze state),
                  blinky = moveAlgorithm (blinky state) (player state),
                  ticks = ticks state + secs
                }
          }
  where
    inputBuffer (PuckMan pos ib) = [y | (_, y, _) <- ib]

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

-- when a key is pressed, move player based on which key is pressed
makeMove :: Player -> Maze -> Player
makeMove p@(PuckMan pos ibs) m = PuckMan (move ibs pos) ibs
  where
    -- applies a move to a position
    move :: [InputBuffer] -> (Position -> Position)
    move ((_, t, a) : ibs') pos'
      | t == Depressed = if moveAllowed p m then getMove a pos' else pos
      | otherwise = move ibs' pos'
    move [] pos' = pos'

    moveAllowed :: Player -> Maze -> Bool
    moveAllowed (PuckMan pos'' _) (t:ts) = case t of
      (Wall posW _) -> not $ inSquare pos'' posW
      (Floor posF) -> inSquare pos'' posF

    intersect :: Position -> [Float] -> Bool
    intersect (px', py') [xR, xL, yU, yD] = px' > xL && px' < xR && py' > yD && py' < yU
    inSquare :: Position -> Position -> Bool
    inSquare p tPos = p `intersect` square tPos

    square (x, y) = [x + tileSize, x - tileSize, y + tileSize, y - tileSize]

-- Updates the input buffer of a player when a key is pressed
updateInputBuffer :: Char -> Player -> Player
updateInputBuffer c (PuckMan pos ibs) = PuckMan pos (updateInputBuffer' c ibs)
  where
    -- Updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer' :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer' c [] = []
    updateInputBuffer' c (ib@(k, t, a) : ibs)
      | c == k = (k, Depressed, a) : updateInputBuffer' c ibs
      | otherwise = (k, Released, a) : updateInputBuffer' c ibs
