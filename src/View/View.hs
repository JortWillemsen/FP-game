-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Data.Maybe (mapMaybe)
import Model.Ghost (Ghost (Ghost), GhostType (Blinky, Pinky, Inky, Clyde), Wellbeing (Scattered, Normal, Frightened), Time)
import Graphics.Gloss
import Model.Maze (Collectable (Dot, Energizer), CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize, FloorType (Trapdoor))
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
showAll ws@WorldState {gameState = state, textures = allTextures, animation = allAnimations}
  | toggled $ menuState state = translate 0 0 (color green (circle 5)) 
  | isPaused state == Pause = Pictures $ showMaze state allTextures allAnimations (time state) ++ [showPlayer state allAnimations,
                                  showGhosts state allAnimations, showLives ws state,
                                  showScore ws state] ++ [showPause ws allTextures]

  | otherwise = Pictures $ showMaze state allTextures allAnimations (time state) ++ [showPlayer state allAnimations,
                                  showGhosts state allAnimations, showLives ws state,
                                  showScore ws state]

showLives :: WorldState -> GameState -> Picture
showLives ws state = translate y x (Color red $ Scale 0.2 0.2 $ Text (show $ lives state))
  where
    c = calculateScreenSize ws
    x = fromIntegral $ fst c + 20
    y = fromIntegral $ snd c `div` 2

showPause :: WorldState -> AllTextures -> Picture
showPause ws text = translate 225 225 (paused $ textTextures text)

showScore :: WorldState -> GameState -> Picture
showScore ws state = translate y x (Color white $ Scale 0.2 0.2 $ Text (show $ score state))
  where
    c = calculateScreenSize ws
    x = fromIntegral $ fst c + 20
    y = fromIntegral $ snd c `div` 2 - 230

showPlayer :: GameState -> AllAnimations -> Picture
showPlayer gstate animations = case player gstate of
  (Player PuckMan (x, y) _ _) -> translate x y $ rotation $ animateTexture anim (time gstate) where
    anim = eatAnim animations
    rotation = case direction $ player gstate of
      U -> rotate (-90)
      D -> rotate 90
      L -> scale (-1) 1
      R -> scale 1 1

showGhosts :: GameState -> AllAnimations -> Picture
showGhosts gstate animations = Pictures [showGhost $ blinky gstate, showGhost $ pinky gstate, showGhost $ inky gstate, showGhost $ clyde gstate] where
  showGhost :: Ghost -> Picture
  showGhost (Ghost t (x, y) _ _ w _) = translate x y $ animateTexture anim (time gstate) 
    where 
      anim = case w of
        Frightened _ -> frightenedAnim animations
        otherwise -> case t of
          Blinky -> blinkyAnim animations
          Pinky -> pinkyAnim animations
          Inky -> inkyAnim animations
          Clyde -> clydeAnim animations

showMaze :: GameState -> AllTextures -> AllAnimations -> Time -> [Picture]
showMaze s@GameState {maze = m} textures animations time = mapMaybe (\x -> loadTile x textures animations time) m

loadTile :: Tile -> AllTextures -> AllAnimations -> Time -> Maybe Picture
loadTile (Floor _ (x, y) (Just cType) _) textures animations time = Just $ translate x y (f cType)
  where
    f Energizer = animateTexture (energizerAnim animations) time
    f Dot = dot $ collectibleTextures textures

loadTile (Floor Trapdoor (x, y) _ _) textures _ _ = Just $ translate x y (trapdoor $ wallTextures textures) 
loadTile (Floor {}) _ _ _ = Nothing
loadTile (Wall (x, y) Nothing) textures _ _ = Nothing
loadTile (Wall (x, y) (Just wtype)) textures _ _ = Just $ translate x y (f wtype $ wallTextures textures)
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