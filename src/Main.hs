module Main where

import Controller
import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Game
import Model
import View
import World
import Maze (getMazeSize)
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  state <- initialWorldState
  playIO
    (InWindow "Puck-Man" (calculateScreenSize state) (0,0)) -- Or FullScreen
    black -- Background color
    20 -- Frames per second
    state -- Initial state
    view
    input -- Event function
    step -- Step function
