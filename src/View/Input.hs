module View.Input where

import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up), Modifiers (shift, ctrl))
import Model.Model
import View.World
import Model.Player (Player(Player))
import Model.Move ( InputBuffer, Toggled(Released, Depressed) )
import GHC.Real (fromIntegral)
import View.World (createCustomWorldState)
import Debug.Trace

toggle :: Toggled -> Toggled
toggle t | t == Released = Depressed
         | otherwise = Released

input :: Event -> WorldState -> IO WorldState
input e ws@WorldState {gameState = state} = handleKey e state ws

-- Handle pause key
handleKey :: Event -> GameState -> WorldState -> IO WorldState
handleKey (EventKey (Char c) t m _) state ws
  | c == 'p' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = toggle (pauseToggle $ screenState state)
                                                        , menuToggle = Released
                                                        , highscoreToggle = Released}}}
  | c == 'm' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = Depressed
                                                                                    , menuToggle = toggle (menuToggle $ screenState state)
                                                                                    , highscoreToggle = Released}}}
  | c == 'h' && t == Down = return ws {gameState = state {screenState = ScreenState { pauseToggle = Depressed
                                                                                    , menuToggle = Released
                                                                                    , highscoreToggle = toggle (highscoreToggle $ screenState state)}}}
  | c `elem` ['1', '2', '3', '4', '5'] = case ctrl m of 
    Down -> if menuToggle (screenState state) == Depressed then createCustomWorldState ws (read [c]) 
           else return ws {gameState = state }
    Up -> if menuToggle (screenState state) == Depressed
                                          then createWorldState (read [c]) 
                                          else return ws {gameState = state }
  | c `elem` ['w', 'a', 's', 'd'] = return ws {gameState = state {player = updateInputForPlayer c (player state)}}
  | otherwise = return ws {gameState = state }
handleKey _ state ws = return ws {gameState = state }

-- updates the input buffer of a player when a key is pressed
updateInputForPlayer :: Char -> Player -> Player
updateInputForPlayer c (Player s pos ibs d sp) = Player s pos (updateInputBuffer c ibs) d sp
  where
    -- updates the input buffer list of a player, making sure one key is depressed at a time
    updateInputBuffer :: Char -> [InputBuffer] -> [InputBuffer]
    updateInputBuffer c [] = []
    updateInputBuffer c (ib@(k, t, a) : ibs)
      | c == k = (k, Depressed, a) : updateInputBuffer c ibs
      | otherwise = (k, Released, a) : updateInputBuffer c ibs
