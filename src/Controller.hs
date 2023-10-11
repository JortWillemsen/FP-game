module Controller where

import Model
import Player ( Player(PuckMan) )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Moveable (up, down, left, right)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs state | ticks state + secs > interval
  = case isKeyPressed state of
      Nothing -> return $ state {
        ticks = ticks state + secs
        }
      Just c -> return $ state {
        player = makeMove (player state) c,
        infoToShow = ShowPlayer,
        ticks = ticks state + secs
        }

-- If input is received, return changed game state
input :: Event -> GameState -> IO GameState
input e state = return (inputKey e state)

-- Check if a key is pressed down change state, otherwise leave state as it was
inputKey :: Event -> GameState -> GameState 
inputKey (EventKey (Char c) pressed _ _) state | pressed == Down = state { isKeyPressed = Just c }
                                               | otherwise = state 
inputKey _ state = state

-- When a key is pressed, move puck-man based on which key is pressed
makeMove :: Player -> Char -> Player
makeMove p c | c == 'w' = up p 
              | c == 's' = down p 
              | c == 'a' = left p 
              | c == 'd' = right p 
              | otherwise = p