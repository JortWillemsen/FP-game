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
view ws = return . showAll $ ws

showAll :: WorldState -> Picture
showAll ws@WorldState {gameState = state, textures = allTextures} = Pictures $ [showPlayer state, showGhost state] ++ (showMaze state allTextures)

showPlayer :: GameState -> Picture
showPlayer gstate = case player gstate of
  (PuckMan (x, y) _) -> translate x y (color yellow (circle 30))

showGhost gstate = case blinky gstate of
  (Blinky (x, y)) -> translate x y (color green (circle 20))

showMaze :: GameState -> AllTextures -> [Picture]
showMaze s@GameState {maze = m} textures@AllTextures {wallTextures = wTextures} = concatMap (\row -> mapMaybe (\tile -> loadTile tile wTextures) row) m

loadTile :: Tile -> WallTextures -> Maybe Picture
loadTile (Wall (x, y) (Just (Corner Nw))) wt@WallTextures {cornerNw = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Corner Ne))) wt@WallTextures {cornerNe = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Corner Sw))) wt@WallTextures {cornerSw = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Corner Se))) wt@WallTextures {cornerSe = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Edge N))) wt@WallTextures {edgeN = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Edge E))) wt@WallTextures {edgeE = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Edge S))) wt@WallTextures {edgeS = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Edge W))) wt@WallTextures {edgeW = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Stump N))) wt@WallTextures {stumpN = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Stump E))) wt@WallTextures {stumpE = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Stump S))) wt@WallTextures {stumpS = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Stump W))) wt@WallTextures {stumpW = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Pipe H))) wt@WallTextures {pipeH = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just (Pipe V))) wt@WallTextures {pipeV = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Wall (x, y) (Just Contained)) wt@WallTextures {contained = txtr} = Just $ translate (x * textureSize) (y * textureSize) txtr
loadTile (Floor _) _ = Nothing