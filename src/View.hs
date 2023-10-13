-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Ghost (Ghost(..))

view :: GameState -> IO Picture
view = return . showAll 

showAll gstate = Pictures [showPlayer gstate, showGhost gstate]

showPlayer gstate = case player gstate of 
    (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))

showGhost gstate = case blinky gstate of 
    (Blinky (x, y)) -> translate x y (color green (circle 20))

-- viewPure :: GameState -> Picture
-- viewPure gstate =   
--   case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowPlayer    -> case player gstate of 
--     (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))
--   ShowGhost     -> case blinky gstate of 
--     (Blinky (x, y)) -> translate x y (color green (circle 20))

    