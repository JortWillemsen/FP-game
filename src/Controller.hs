module Controller where

import Model
import Player ( Player(PuckMan), Toggled (Depressed, Released), InputBuffer, resetInputBuffer )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Move (up, down, left, right, Association (..), Position, getMove)
import Ghost (moveAlgorithm)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs state | isPaused state == Pause = return $ state {
                  ticks      = ticks state + secs
                  }
                | otherwise               = return $ state {
                  player     = makeMove (player state),
                  blinky     = moveAlgorithm (blinky state) (player state),
                  ticks      = ticks state + secs
                  }
  where
    inputBuffer (PuckMan pos ib) = [y | (_, y, _) <- ib]


-- If input is received, return changed game state
input :: Event -> GameState -> IO GameState
input e state = return (inputKey e state)


-- Check if a key is pressed down change state, otherwise leave state as it was
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) t _ _) state
  | c == 'p' && t == Down = state { isPaused = pauseGame (isPaused state) }
  | c == 'p' && t == Up   = state
  | otherwise             = state { player = updateInputBuffer c (player state) }
inputKey _ state          = state


-- when a key is pressed, move player based on which key is pressed
makeMove :: Player -> Player
makeMove (PuckMan pos ibs) = PuckMan (move ibs pos) ibs
  where
    -- applies a move to a position 
    move :: [InputBuffer] -> (Position -> Position)
    move ((_, t, a):ibs') pos' | t == Depressed = getMove a pos'
                               | otherwise      = move ibs' pos'
    move [] pos' = pos'
    

-- Updates the input buffer of a player when a key is pressed 
updateInputBuffer :: Char -> Player -> Player
updateInputBuffer c (PuckMan pos ibs) = PuckMan pos (updateInputBuffer' c ibs)
  where
    -- Updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer' :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer' c [] = []
    updateInputBuffer' c (ib@(k, t, a):ibs) | c == k    = (k, Depressed, a) : updateInputBuffer' c ibs
                                            | otherwise = (k, Released, a) : updateInputBuffer' c ibs
