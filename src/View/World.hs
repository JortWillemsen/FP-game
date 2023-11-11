module View.World where

import Graphics.Gloss
import Model.Model
import View.Animation
import View.File( loadLevel, loadCustomLevel, loadHighScores ) 
import View.Random (generateSeed)
import System.Random (StdGen)

data WorldState = WorldState
  { gameState :: GameState,
    highScores :: [String],
    textures :: AllTextures,
    animation :: AllAnimations
  }

createWorldState :: Level -> IO WorldState
createWorldState l =
  do
    textures <- loadTextures
    level <- loadLevel l 
    highscores <- loadHighScores
    seed <- generateSeed
    WorldState (basicState level l seed) highscores textures <$> loadAnimations

createCustomWorldState :: WorldState -> Level -> IO WorldState
createCustomWorldState ws l =
  do
    textures <- loadTextures
    level <- loadCustomLevel l
    highscores <- loadHighScores 
    seed <- generateSeed

    if not (null level)
      then WorldState (basicState level l seed) highscores textures <$> loadAnimations
      else return ws

data AllTextures = AllTextures
  { wallTextures :: WallTextures,
    textTextures :: TextTextures,
    collectibleTextures :: CollectibleTextures,
    playerTexture :: Texture
  }

data AllAnimations = AllAnimations
  { eatAnim :: Animation,
    blinkyAnim :: Animation,
    pinkyAnim :: Animation,
    inkyAnim :: Animation,
    clydeAnim :: Animation,
    frightenedAnim :: Animation,
    eyesAnim :: Animation,
    energizerAnim :: Animation
  }

data TextTextures = TextTextures {
  paused :: Texture,
  menu :: Texture
  }

data WallTextures = WallTextures
  { cornerNw :: Texture,
    cornerNe :: Texture,
    cornerSw :: Texture,
    cornerSe :: Texture,
    edgeN :: Texture,
    edgeE :: Texture,
    edgeS :: Texture,
    edgeW :: Texture,
    stumpN :: Texture,
    stumpE :: Texture,
    stumpS :: Texture,
    stumpW :: Texture,
    pipeH :: Texture,
    pipeV :: Texture,
    contained :: Texture,
    trapdoor :: Texture
  }

data CollectibleTextures = CollectibleTextures
  { dot :: Texture,
    energizer :: Texture
  }

loadAnimations :: IO AllAnimations
loadAnimations =
  do
    eatFrames <- mapM loadBMP ["Assets/animations/eat/frame1.bmp", "Assets/animations/eat/frame2.bmp", "Assets/animations/eat/frame3.bmp"]
    blinkyFrames <- mapM loadBMP ["Assets/animations/ghost/Blinky1.bmp", "Assets/animations/ghost/Blinky2.bmp"]
    pinkyFrames <- mapM loadBMP ["Assets/animations/ghost/Pinky1.bmp", "Assets/animations/ghost/Pinky2.bmp"]
    inkyFrames <- mapM loadBMP ["Assets/animations/ghost/Inky1.bmp", "Assets/animations/ghost/Inky2.bmp"]
    clydeFrames <- mapM loadBMP ["Assets/animations/ghost/Clyde1.bmp", "Assets/animations/ghost/Clyde2.bmp"]
    scatteredFrames <- mapM loadBMP ["Assets/animations/ghost/Scattered1.bmp", "Assets/animations/ghost/Scattered2.bmp"]
    eyesFrames <- mapM loadBMP ["Assets/animations/ghost/eyes.bmp"]
    energizerFrames <- mapM loadBMP ["Assets/collectibles/energizer1.bmp", "Assets/collectibles/energizer2.bmp"]
    let eat = Animation 0.5 eatFrames
    let blinky = Animation 0.5 blinkyFrames
    let pinky = Animation 0.5 pinkyFrames
    let inky = Animation 0.5 inkyFrames
    let clyde = Animation 0.5 clydeFrames
    let scattered = Animation 0.5 scatteredFrames
    let energizer = Animation 0.3 energizerFrames
    let eyes = Animation 2 eyesFrames

    return $ AllAnimations eat blinky pinky inky clyde scattered eyes energizer

-- Loading all the bitmaps using monads (<$> and <*> are from applicative)
loadTextures :: IO AllTextures
loadTextures =
  do
    textTextures <-
      TextTextures
        <$> loadBMP "Assets/text/paused.bmp"
        <*> loadBMP "Assets/text/menu.bmp"
    playerTexture <- loadBMP "Assets/player/puck-man.bmp"
    collectibleTextures <-
      CollectibleTextures
        <$> loadBMP "Assets/collectibles/dot.bmp"
        <*> loadBMP "Assets/collectibles/energizer1.bmp"
    wallTextures <-
      WallTextures
        <$> loadBMP "Assets/walls/wall_corner_tl.bmp"
        <*> loadBMP "Assets/walls/wall_corner_tr.bmp"
        <*> loadBMP "Assets/walls/wall_corner_bl.bmp"
        <*> loadBMP "Assets/walls/wall_corner_br.bmp"
        <*> loadBMP "Assets/walls/wall_edge_tp.bmp"
        <*> loadBMP "Assets/walls/wall_edge_r.bmp"
        <*> loadBMP "Assets/walls/wall_edge_btm.bmp"
        <*> loadBMP "Assets/walls/wall_edge_l.bmp"
        <*> loadBMP "Assets/walls/wall_stump_tp.bmp"
        <*> loadBMP "Assets/walls/wall_stump_r.bmp"
        <*> loadBMP "Assets/walls/wall_stump_btm.bmp"
        <*> loadBMP "Assets/walls/wall_stump_l.bmp"
        <*> loadBMP "Assets/walls/wall_pipe_h.bmp"
        <*> loadBMP "Assets/walls/wall_pipe_v.bmp"
        <*> loadBMP "Assets/walls/wall_contained.bmp"
        <*> loadBMP "Assets/walls/trapdoor.bmp"

    -- Creating the all textures structure with all the textures loaded.
    return $ AllTextures wallTextures textTextures collectibleTextures playerTexture
