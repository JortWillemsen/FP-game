module Main where

import Controller.Controller
import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game
import Model.Maze (getMazeSize)
import Model.Model
import View.View ( calculateScreenSize, view )
import View.World
import View.Input

main :: IO ()
main = do
  state <- initialWorldState
  playIO
    (InWindow "Puck-Man" (calculateScreenSize state) (0, 0)) -- Or FullScreen
    black -- Background color
    30 -- Frames per second
    state -- Initial state
    view
    input -- Event function
    step -- Step function
