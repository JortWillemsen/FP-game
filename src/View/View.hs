-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Data.Maybe (mapMaybe)
import Model.Ghost (Ghost (Ghost))
import Graphics.Gloss
import Model.Maze (Collectable (Dot, Energizer), CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize)
import Model.Model
import Model.Move
import Model.Player
import View.World
import View.Animation
import Debug.Trace
import View.File (loadHighScores)

textureSize :: Float
textureSize = 16.0

scalingFactor :: Float
scalingFactor = 1.5

centerOfMaze :: (Float, Float) -> (Float, Float)
centerOfMaze (x, y) = (abs x / 2, abs y / 2 - 25)

calculateScreenSize :: WorldState -> (Int, Int)
calculateScreenSize ws = let (x, y) = getMazeSize (maze $ gameState ws) in (round (x * scalingFactor), round (y * scalingFactor + 40))

offset :: (Int, Int) -> (Float, Float)
offset (x, y) = (-(fromIntegral x / 2), -(fromIntegral y / 2))

view :: WorldState -> IO Picture
view ws = let (x, y) = s in return $ translate x y $ scale scalingFactor scalingFactor $ showAll s ws
  where
    s = offset $ calculateScreenSize ws

showAll :: (Float, Float) -> WorldState -> Picture
showAll s ws@WorldState {gameState = state, textures = allTextures, animation = allAnimations}
  | menuToggle (screenState state) == Depressed = showMenu s allTextures
  | highscoreToggle (screenState state) == Depressed = showHighScores s ws
  | pauseToggle (screenState state) == Depressed = Pictures $ base ++ [showPause s allTextures]
  | otherwise = Pictures base
    where
      base = showMaze state allTextures ++ [showPlayer state allAnimations,
                                  showGhost state, showLives s state,
                                  showScore s state]

showLives :: (Float, Float) -> GameState -> Picture -- Hearts
showLives s@(x, y) state = translate (abs x - 20) (abs $ y + 10) (Color red $ Scale 0.2 0.2 $ Text (show $ lives state))

showPause :: (Float, Float) -> AllTextures -> Picture
showPause s text = translate x y (paused $ textTextures text)
  where
    (x, y) = centerOfMaze s

showScore :: (Float, Float) -> GameState -> Picture
showScore (x, y) state = translate (abs x + x + 10) (abs $ y + 10) (Color white $ Scale 0.2 0.2 $ Text (show $ score state))

showMenu :: (Float, Float) -> AllTextures -> Picture
showMenu s text = translate x y (menu $ textTextures text)
  where
    (x, y) = centerOfMaze s

showHighScores :: (Float, Float) -> WorldState -> Picture
showHighScores s ws = translate x y (Color white $ Scale 0.2 0.2 $ Text ("HIGHSCORES" ++ "\n" ++ text))
  where 
    text = text' (highScores ws)
      where 
        text' :: [String] -> [Char]
        text' [] = []
        text' (line:lines) = line ++ "\n" ++ text' lines
    (x, y) = centerOfMaze s 

showPlayer :: GameState -> AllAnimations -> Picture
showPlayer gstate animations = case player gstate of
  (Player PuckMan (x, y) _ _) -> translate x y $ rotation $ animateTexture anim (time gstate) where
    anim = eat animations
    rotation = case direction $ player gstate of
      U -> rotate (-90)
      D -> rotate 90
      L -> scale (-1) 1
      R -> scale 1 1

showGhost :: GameState -> Picture
showGhost gstate = case blinky gstate of
  (Ghost _ (x, y) _ _) -> translate x y (color green (circle 5))

showMaze :: GameState -> AllTextures -> [Picture]
showMaze s@GameState {maze = m} textures = mapMaybe (`loadTile` textures) m

loadTile :: Tile -> AllTextures -> Maybe Picture
loadTile (Floor (x, y) (Just cType) _) textures = Just $ translate x y (f cType $ collectibleTextures textures)
  where
    f Energizer = energizer
    f Dot = dot
loadTile (Floor {}) _ = Nothing
loadTile (Wall (x, y) Nothing) textures = Nothing
loadTile (Wall (x, y) (Just wtype)) textures = Just $ translate x y (f wtype $ wallTextures textures)
  where
    f (Corner Nw) = cornerNw
    f (Corner Ne) = cornerNe
    f (Corner Sw) = cornerSw
    f (Corner Se) = cornerSe
    f (Edge N) = edgeN
    f (Edge E) = edgeE
    f (Edge S) = edgeS
    f (Edge W) = edgeW
    f (Stump N) = stumpN
    f (Stump E) = stumpE
    f (Stump S) = stumpS
    f (Stump W) = stumpW
    f (Pipe H) = pipeH
    f (Pipe V) = pipeV
    f Contained = contained