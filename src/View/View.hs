-- | This module defines how to turn
--   the game state into a picture
module View.View where

import Data.Maybe (mapMaybe)
import Graphics.Gloss
  ( Picture (Color, Pictures, Scale, Text),
    red,
    rotate,
    scale,
    translate,
    white,
  )
import Model.Constants (scalingFactor, textSize, xLivesOffset, xScoreOffset, xHighScoreOffset, yHighScoreOffset, degrees90, factor)
import Model.Ghost (Ghost (Ghost), GhostType (Blinky, Clyde, Inky, Pinky), Wellbeing (Frightened, Normal, Respawning, Scattered, Spawning))
import Model.Maze (Collectable (Dot, Energizer), CornerDirection (Ne, Nw, Se, Sw), EdgeDirection (E, N, S, W), FloorType (Trapdoor), Maze, PipeDirection (H, V), Tile (Floor, Wall), WallType (Contained, Corner, Edge, Pipe, Stump), getMazeSize)
import Model.Model
  ( GameState
      ( GameState,
        blinky,
        clyde,
        inky,
        lives,
        maze,
        pinky,
        player,
        score,
        screenState,
        time
      ),
    Screen (Show),
    ScreenState (highscoreToggle, menuToggle, pauseToggle),
    Time,
  )
import Model.Move (Direction (D, L, R, U))
import Model.Player
  ( Player (Player, direction),
    PlayerType (PuckMan),
  )
import View.Animation (animateTexture)
import View.File
  ( AllAnimations
      ( blinkyAnim,
        clydeAnim,
        eatAnim,
        energizerAnim,
        eyesAnim,
        frightenedAnim,
        inkyAnim,
        pinkyAnim
      ),
    AllTextures (collectibleTextures, textTextures, wallTextures),
    CollectibleTextures (dot),
    TextTextures (menu, paused),
    WallTextures
      ( contained,
        cornerNe,
        cornerNw,
        cornerSe,
        cornerSw,
        edgeE,
        edgeN,
        edgeS,
        edgeW,
        pipeH,
        pipeV,
        stumpE,
        stumpN,
        stumpS,
        stumpW,
        trapdoor
      ),
  )
import View.World (WorldState (..))

-- | Calculate where the center of the maze is so we can center it in the screen
centerOfMaze :: (Float, Float) -> (Float, Float)
centerOfMaze (x, y) = (abs x / scalingFactor, abs y / scalingFactor)

-- | Calculates the total screen size
calculateScreenSize :: WorldState -> (Int, Int)
calculateScreenSize ws = let (x, y) = getMazeSize (maze $ gameState ws) in (round (x * scalingFactor), round (y * scalingFactor))

-- | Calculates how much we need to offset the maze to center it
offset :: (Int, Int) -> (Float, Float)
offset (x, y) = (-(fromIntegral x / 2), -(fromIntegral y / 2))

-- | Returns the complete view to gloss
view :: WorldState -> IO Picture
view ws = let (x, y) = offset $ calculateScreenSize ws in return $ translate x y $ scale scalingFactor scalingFactor $ showAll s ws
  where
    s = offset $ calculateScreenSize ws

-- | Renders all states of the game
showAll :: (Float, Float) -> WorldState -> Picture
showAll s ws@WorldState {gameState = state, textures = allTextures, animation = allAnimations}
  | menuToggle (screenState state) == Show = showMenu s allTextures
  | highscoreToggle (screenState state) == Show = showHighScores s ws
  | pauseToggle (screenState state) == Show = Pictures $ base ++ [showPause s allTextures]
  | otherwise = Pictures base
  where
    base =
      showMaze state allTextures allAnimations (time state)
        ++ [ showPlayer state allAnimations,
             showGhosts state allAnimations,
             showLives s state,
             showScore s state
           ]

-- | Shows the lives at the top of the screen whilest playing
showLives :: (Float, Float) -> GameState -> Picture 
showLives s state = translate (abs x + x - xLivesOffset) (abs y + y) (Color red $ Scale textSize textSize $ Text ("Lives: " ++ show (lives state)))
  where
    (x, y) = centerOfMaze s

-- | Shows the pause screen
showPause :: (Float, Float) -> AllTextures -> Picture
showPause s text = translate x y (paused $ textTextures text)
  where
    (x, y) = centerOfMaze s

-- | Shows the score at the top of the screen
showScore :: (Float, Float) -> GameState -> Picture
showScore s state = translate (abs x - x + xScoreOffset) (abs y + y) (Color white $ Scale textSize textSize $ Text ("Score: " ++ show (score state)))
  where
    (x, y) = centerOfMaze s

-- | Shows the main menu
showMenu :: (Float, Float) -> AllTextures -> Picture
showMenu s text = translate x y (menu $ textTextures text)
  where
    (x, y) = centerOfMaze s

-- | Shows the high score screen
showHighScores :: (Float, Float) -> WorldState -> Picture
showHighScores s ws =
  Pictures
    [ translate (x / 2 + xHighScoreOffset) (y + y) (Color red $ Scale textSize textSize $ Text "HIGHSCORES"),
      translate (x / 2) (y + y - yHighScoreOffset) (Color white $ Scale textSize textSize text)
    ]
  where
    text = Pictures $ zipWith (\line y -> translate (x / 2) y (Text line)) (highScores ws) [0, -120 ..]
    (x, y) = centerOfMaze s

-- | Shows the player
showPlayer :: GameState -> AllAnimations -> Picture
showPlayer gstate animations = case player gstate of
  (Player PuckMan (x, y) _ _ _) -> translate x y $ rotation $ animateTexture anim (time gstate)
    where
      anim = eatAnim animations

      -- Rotate the player based on its direction
      rotation = case direction $ player gstate of
        U -> rotate (-degrees90)
        D -> rotate degrees90
        L -> scale (-factor) factor
        R -> scale factor factor

-- | Shows the ghosts
showGhosts :: GameState -> AllAnimations -> Picture
showGhosts gstate animations = Pictures [showGhost $ blinky gstate, showGhost $ pinky gstate, showGhost $ inky gstate, showGhost $ clyde gstate]
  where
    showGhost :: Ghost -> Picture
    showGhost (Ghost t (x, y) _ _ _ w) = translate x y $ animateTexture anim (time gstate)
      where
        -- Change the animation based on the wellbeing of the ghost
        anim = case w of
          Frightened _ -> frightenedAnim animations
          Respawning -> eyesAnim animations
          _ -> case t of
            Blinky -> blinkyAnim animations
            Pinky -> pinkyAnim animations
            Inky -> inkyAnim animations
            Clyde -> clydeAnim animations

-- | Shows the maze
showMaze :: GameState -> AllTextures -> AllAnimations -> Time -> [Picture]
showMaze s@GameState {maze = m} textures animations time = mapMaybe (\x -> loadTile x textures animations time) m

-- | Shows a tile
loadTile :: Tile -> AllTextures -> AllAnimations -> Time -> Maybe Picture
loadTile (Floor _ (x, y) (Just cType) _) textures animations time = Just $ translate x y (f cType)
  where
    -- If the tile is a floor we need to render either an energizer or a dot

    f Energizer = animateTexture (energizerAnim animations) time
    f Dot = dot $ collectibleTextures textures
loadTile (Floor Trapdoor (x, y) _ _) textures _ _ = Just $ translate x y (trapdoor $ wallTextures textures)
loadTile (Floor {}) _ _ _ = Nothing
loadTile (Wall (x, y) Nothing) textures _ _ = Nothing
loadTile (Wall (x, y) (Just wtype)) textures _ _ = Just $ translate x y (f wtype $ wallTextures textures)
  where
    -- We need to render the texture corresponding to the wall type
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