module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)

main :: IO ()
main = do
    playIO (InWindow "Puck-Man" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              20               -- Frames per second
              initialState     -- Initial state
              view
              input            -- Event function
              step             -- Step function

