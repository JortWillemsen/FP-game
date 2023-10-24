module Animation where
import Graphics.Gloss (Picture)
import GHC.Float (int2Float)
import Model (Time)
import Data.Fixed (mod')

data Animation = Animation Duration [Texture]
type Duration = Float
type Texture = Picture

-- Finds the frame of the animation we are currently on based on the time
animateTexture :: Animation -> Time -> Texture
animateTexture (Animation d frames) e = frames!!cf where
  -- Calculates the duration of every frame
  dpf :: Float -> Int -> Float
  dpf duration frames
    | duration == 0 || frames == 0 = 0
    | otherwise = duration / int2Float frames
  
  -- Calculates the time since the start of last animation run
  tss :: Float -> Float -> Float
  tss duration elapsed 
    | duration == 0 || elapsed == 0 = 0
    | elapsed < duration = elapsed
    | otherwise = elapsed `mod'` duration
  
  -- Calculates what frame we are on based on the current duration
  cf :: Int
  cf = floor (tss d e / dpf d (length frames))
  


-- 30 seconden sinds begin van applicatie, animatie duurt 4 seconden

