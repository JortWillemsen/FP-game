-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowPlayer    -> case player gstate of 
    (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))

    