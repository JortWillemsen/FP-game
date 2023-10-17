-- | This module defines how to turn
--   the game state into a picture
module View where

import Data.Maybe (mapMaybe)
import Ghost (Ghost (..))
import Graphics.Gloss
import Maze (CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump))
import Model
import Player
import World

textureSize :: Float
textureSize = 16.0

view :: WorldState -> IO Picture
view = return . showAll

showAll :: WorldState -> Picture
showAll ws@WorldState {gameState = state, textures = allTextures} = Pictures $ [showPlayer state, showGhost state] ++ (showMaze state allTextures)

showPlayer :: GameState -> Picture
showPlayer gstate = case player gstate of
  (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))

showGhost gstate = case blinky gstate of
  (Blinky (x, y)) -> translate x y (color green (circle 20))

showMaze :: GameState -> AllTextures -> [Picture]
showMaze s@GameState {maze = m} textures@AllTextures {wallTextures = wTextures} = mapMaybe (\tile -> loadTile tile wTextures) m

loadTile :: Tile -> WallTextures -> Maybe Picture
loadTile (Floor _) _ = Nothing
loadTile (Wall (x, y) Nothing) wt = Nothing
loadTile (Wall (x, y) (Just wtype)) wt = Just $ translate (x * textureSize) (y * textureSize) (f wtype $ wt)
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