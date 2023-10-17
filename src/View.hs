-- | This module defines how to turn
--   the game state into a picture
module View where

import Data.Maybe (mapMaybe)
import Ghost (Ghost (..))
import Graphics.Gloss
import Maze (CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize, Collectable (Energizer, Dot))
import Model
import Move
import Player
import World

textureSize :: Float
textureSize = 16.0

scalingFactor :: Float
scalingFactor = 3.0

calculateScreenSize :: WorldState -> (Int, Int)
calculateScreenSize ws = let (x, y) = getMazeSize (maze $ gameState ws) in (round (x*scalingFactor), round (y*scalingFactor))

offset :: (Int, Int) -> (Float, Float)
offset (x , y) = (-(fromIntegral x / 2), -(fromIntegral y / 2)) 

view :: WorldState -> IO Picture
view ws = let (x, y) = offset $ calculateScreenSize ws in return $ translate x y $ scale scalingFactor scalingFactor $ showAll ws

showAll :: WorldState -> Picture
showAll ws@WorldState {gameState = state, textures = allTextures} = Pictures $ [showPlayer state, showGhost state] ++ (showMaze state allTextures)

showPlayer :: GameState -> Picture
showPlayer gstate = case player gstate of
  (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))

showGhost gstate = case blinky gstate of
  (Blinky (x, y)) -> translate x y (color green (circle 20))

showMaze :: GameState -> AllTextures -> [Picture]
showMaze s@GameState {maze = m} textures = mapMaybe (\tile -> loadTile tile textures) m

loadTile :: Tile -> AllTextures -> Maybe Picture
loadTile (Floor (x, y) (Just cType)) textures = Just $ translate x y (f cType $ collectibleTextures textures) 
  where
    f Energizer = energizer
    f Dot = dot
loadTile (Floor _ _) _ = Nothing
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