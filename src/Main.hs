module Main where

import Controller
import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Game
import Model
import View
import World

main :: IO ()
main = do
  state <- initialWorldState
  playIO
    (InWindow "Puck-Man" (800, 800) (0, 0)) -- Or FullScreen
    black -- Background color
    20 -- Frames per second
    state -- Initial state
    view
    input -- Event function
    step -- Step function
