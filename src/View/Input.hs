module View.Input where

import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up), Modifiers (shift, ctrl))
import Model.Model
import View.World
import Model.Player (Player(Player))
import Model.Move ( InputBuffer, Toggled(Released, Depressed) )
import GHC.Real (fromIntegral)
import View.World (createCustomWorldState)
import Debug.Trace

input :: Event -> WorldState -> IO WorldState
input e ws@WorldState {gameState = state} = handleKey e state ws

-- | Handle key events
handleKey :: Event -> GameState -> WorldState -> IO WorldState
handleKey (EventKey (Char c) t m _) state ws
  -- P = show the paused screen
  | c == 'p' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = toggleScreen (pauseToggle $ screenState state)
                                                        , menuToggle = Hide
                                                        , highscoreToggle = Hide}}}
  -- M = Show the menu screen
  | c == 'm' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = Show
                                                                                    , menuToggle = toggleScreen (menuToggle $ screenState state)
                                                                                    , highscoreToggle = Hide}}}
  -- H = Show the high scores screen
  | c == 'h' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = Show
                                                                                    , menuToggle = Hide
                                                                                    , highscoreToggle = toggleScreen (highscoreToggle $ screenState state)}}}
  -- Number = Load the level associated to that number
  | c `elem` ['1', '2', '3', '4', '5'] = case ctrl m of 
    -- If we hold ctrl we need to load a custom level
    Down -> if menuToggle (screenState state) == Show 
      then createCustomWorldState ws (read [c]) 
      else return ws {gameState = state }
    -- If not, we just load a normal level
    Up -> if menuToggle (screenState state) == Show
      then createWorldState (read [c]) 
      else return ws {gameState = state }
  -- If the input is wasd then we need to update the input buffer of the player
  | c `elem` ['w', 'a', 's', 'd'] = return ws {gameState = state {player = updateInputForPlayer c (player state)}}
  | otherwise = return ws {gameState = state }
handleKey _ state ws = return ws {gameState = state }

-- | Updates the input buffer of a player when a key is pressed
updateInputForPlayer :: Char -> Player -> Player
updateInputForPlayer c (Player s pos ibs d sp) = Player s pos (updateInputBuffer c ibs) d sp
  where
    -- updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer c [] = []
    updateInputBuffer c (ib@(k, t, a) : ibs)
      | c == k = (k, Depressed, a) : updateInputBuffer c ibs
      | otherwise = (k, Released, a) : updateInputBuffer c ibs
