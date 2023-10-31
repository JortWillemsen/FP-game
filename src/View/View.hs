-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Data.Maybe (mapMaybe)
import Model.Ghost (Ghost (Ghost), GhostType (Blinky, Pinky, Inky, Clyde), Wellbeing (Scattered, Normal, Frightened, Spawning))
import Graphics.Gloss
import Model.Maze (Collectable (Dot, Energizer), CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize, FloorType (Trapdoor))
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
centerOfMaze (x, y) = (abs x / scalingFactor, abs y / scalingFactor)

calculateScreenSize :: WorldState -> (Int, Int)
calculateScreenSize ws = let (x, y) = getMazeSize (maze $ gameState ws) in (round (x * scalingFactor), round (y * scalingFactor))

offset :: (Int, Int) -> (Float, Float)
offset (x, y) = (-(fromIntegral x / 2), -(fromIntegral y / 2))

view :: WorldState -> IO Picture
view ws = let (x, y) = offset $ calculateScreenSize ws in return $ translate x y $ scale scalingFactor scalingFactor $ showAll s ws where
  s = offset $ calculateScreenSize ws

showAll :: (Float, Float) -> WorldState -> Picture
showAll s ws@WorldState {gameState = state, textures = allTextures, animation = allAnimations}
  | menuToggle (screenState state) == Depressed = showMenu s allTextures
  | highscoreToggle (screenState state) == Depressed = showHighScores s ws
  | pauseToggle (screenState state) == Depressed = Pictures $ base ++ [showPause s allTextures]
  | otherwise = Pictures base
    where
      base = showMaze state allTextures allAnimations (time state) ++ [showPlayer state allAnimations,
                                  showGhosts state allAnimations, showLives s state,
                                  showScore s state]

showLives :: (Float, Float) -> GameState -> Picture -- Hearts
showLives s state = translate (abs x + x - 110) (abs y + y) (Color red $ Scale 0.2 0.2 $ Text ("Lives: " ++ (show $ lives state)))
  where 
    (x, y) = centerOfMaze s

showPause :: (Float, Float) -> AllTextures -> Picture
showPause s text = translate x y (paused $ textTextures text)
  where
    (x, y) = centerOfMaze s

showScore :: (Float, Float) -> GameState -> Picture
showScore s state = translate (abs x - x + 10) (abs y + y) (Color white $ Scale 0.2 0.2 $ Text ("Score: " ++ (show $ score state)))
  where 
    (x, y) = centerOfMaze s

showMenu :: (Float, Float) -> AllTextures -> Picture
showMenu s text = translate x y (menu $ textTextures text)
  where
    (x, y) = centerOfMaze s

showHighScores :: (Float, Float) -> WorldState -> Picture
showHighScores s ws = translate x y (Color white $ Scale 0.2 0.2 text)
  where
    text = Pictures $ zipWith (\line y -> translate 0 y (Text line)) (highScores ws) [0, - 120..]
    (x, y) = centerOfMaze s

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
  showGhost (Ghost t (x, y) _ _ _ w _) = translate x y $ animateTexture anim (time gstate) 
    where 
      anim = case w of
        Spawning _ -> frightenedAnim animations
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