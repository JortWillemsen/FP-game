module World where

import Graphics.Gloss
import Model

data WorldState = WorldState
  { gameState :: GameState,
    textures :: AllTextures
  }

initialWorldState :: IO WorldState
initialWorldState =
  do
    textures <- loadTextures
    level <- loadLevel
    return $ WorldState (initialState level) textures

type Texture = Picture

data AllTextures = AllTextures
  { wallTextures :: WallTextures,
    collectibleTextures :: CollectibleTextures
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

data CollectibleTextures = CollectibleTextures {
  dot :: Texture,
  energizer :: Texture
}

loadLevel :: IO [String]
loadLevel = do
  level <- readFile "level/level.txt"

  return $ lines level

-- Loading all the bitmaps using monads (<$> and <*> are from applicative)
loadTextures :: IO AllTextures
loadTextures =
  do
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
    return $ AllTextures wallTextures collectibleTextures