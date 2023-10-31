module View.World where

import Graphics.Gloss
import Model.Model
import View.Animation
import View.File ( loadLevel, loadCustomLevel, loadHighScores ) 


data WorldState = WorldState
  { gameState :: GameState,
    highScores :: HighScores,
    textures :: AllTextures,
    animation :: AllAnimations
  } 

createWorldState :: Level -> IO WorldState
createWorldState l =
  do
    textures <- loadTextures
    level <- loadLevel l 
    highscores <- loadHighScores
    WorldState (nextState level l) highscores textures <$> loadAnimations

createCustomWorldState :: Level -> IO WorldState
createCustomWorldState l =
  do
    textures <- loadTextures
    level <- loadCustomLevel l
    highscores <- loadHighScores 
    WorldState (nextState level l) highscores textures <$> loadAnimations


type HighScores = [String]

data AllTextures = AllTextures
  { wallTextures :: WallTextures,
    textTextures :: TextTextures,
    collectibleTextures :: CollectibleTextures,
    playerTexture :: Texture
  }

data AllAnimations = AllAnimations
  { eat :: Animation
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
    contained :: Texture
  }

data CollectibleTextures = CollectibleTextures
  { dot :: Texture,
    energizer :: Texture
  }

loadAnimations :: IO AllAnimations
loadAnimations =
  do
    eatFrames <- mapM loadBMP ["Assets/animations/eat/frame1.bmp", "Assets/animations/eat/frame2.bmp", "Assets/animations/eat/frame3.bmp"]

    let eat = Animation 0.5 eatFrames

    return $ AllAnimations eat
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

    -- Creating the all textures structure with all the textures loaded.
    return $ AllTextures wallTextures textTextures collectibleTextures playerTexture
