module View.Animation where

import Graphics.Gloss (Picture)
import GHC.Float (int2Float)
import Data.Fixed (mod')
import Model.Ghost (Time)
import View.File

-- | Finds the frame of the animation we are currently on based on the time and the length of the animation
animateTexture :: Animation -> Time -> Texture
animateTexture (Animation d frames) e = frames!!cf where
  -- | (duration per frame) - Calculates the duration of every frame of animation
  dpf :: Float -> Int -> Float
  dpf duration frames
    | duration == 0 || frames == 0 = 0
    | otherwise = duration / int2Float frames
  
  -- | (time since start) - Calculates the time since the start of last animation run
  tss :: Float -> Float -> Float
  tss duration elapsed 
    | duration == 0 || elapsed == 0 = 0
    | elapsed < duration = elapsed
    | otherwise = elapsed `mod'` duration
  
  -- | (current frame) - Calculates what frame we are on based on the duration of the current animation run and how long a single frame is
  cf :: Int
  cf = floor (tss d e / dpf d (length frames))
