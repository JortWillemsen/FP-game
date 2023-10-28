module View.World where

import Graphics.Gloss
import Model.Model
import View.Animation
import View.File
import View.Random (generateSeed)
import System.Random (StdGen)

data WorldState = WorldState
  { gameState :: GameState,
    textures :: AllTextures,
    animation :: AllAnimations
  }

createWorldState :: Level -> IO WorldState
createWorldState l =
  do
    textures <- loadTextures
    level <- loadLevel l
    seed <- generateSeed
    WorldState (nextState level l seed) textures <$> loadAnimations

data AllTextures = AllTextures
  { wallTextures :: WallTextures,
    textTextures :: TextTextures,
    collectibleTextures :: CollectibleTextures,
    playerTexture :: Texture
  }

data AllAnimations = AllAnimations
  { eatAnim :: Animation,
    blinkyAnim :: Animation
  }

data TextTextures = TextTextures
  { paused :: Texture
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
    
    let eat = Animation 0.5 eatFrames
    let blinky = Animation 0.5 blinkyFrames

    return $ AllAnimations eat blinky

-- Loading all the bitmaps using monads (<$> and <*> are from applicative)
loadTextures :: IO AllTextures
loadTextures =
  do
    textTextures <-
      TextTextures
        <$> loadBMP "Assets/text/paused.bmp"
    playerTexture <- loadBMP "Assets/player/puck-man.bmp"
    collectibleTextures <-
      CollectibleTextures
        <$> loadBMP "Assets/collectibles/dot.bmp"
        <*> loadBMP "Assets/collectibles/energizer.bmp"
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
