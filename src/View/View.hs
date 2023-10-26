-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Data.Maybe (mapMaybe)
import Model.Ghost (Ghost (..))
import Graphics.Gloss
import Model.Maze (Collectable (Dot, Energizer), CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize)
import Model.Model
import Model.Move
import Model.Player
import View.World
import View.Animation

textureSize :: Float
textureSize = 16.0

scalingFactor :: Float
scalingFactor = 1.5

calculateScreenSize :: WorldState -> (Int, Int)
calculateScreenSize ws = let (x, y) = getMazeSize (maze $ gameState ws) in (round (x * scalingFactor), round (y * scalingFactor + 40))

offset :: (Int, Int) -> (Float, Float)
offset (x, y) = (-(fromIntegral x / 2), -(fromIntegral y / 2))

view :: WorldState -> IO Picture
view ws = let (x, y) = offset $ calculateScreenSize ws in return $ translate x y $ scale scalingFactor scalingFactor $ showAll ws

showAll :: WorldState -> Picture
showAll ws@WorldState {gameState = state, textures = allTextures, animation = allAnimations} = Pictures $ [showPlayer state allAnimations, showGhost state] ++ (showMaze state allTextures)

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
  (Blinky (x, y) _) -> translate x y (color green (circle 5))

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