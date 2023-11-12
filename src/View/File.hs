{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
module View.File where

import Model.Score ( Score, HighScore (HighScore) )
import Model.Model ( Level )
import Control.Monad (when)
import Model.Player (PlayerType (PuckMan))
import Data.List (insert)
import Graphics.Gloss (Picture, loadBMP)
import Model.Constants (highScoreLimit)

type Texture = Picture
type Duration = Float
data Animation = Animation Duration [Texture]

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

-- | Loads the level from a file into a [string] representing the lines in the text file
loadLevel :: Level -> FilePath -> IO [String]
loadLevel i fp = do
    level <- readFile (fp ++ show i ++ ".txt")
    return $ lines level

-- | Loads the high scores file into a [string] representing the lines in the text file
loadHighScores :: IO [String]
loadHighScores = do
    scores <- readFile "score/highscores.txt"
    return $ lines scores

-- | Saves a new high score (ordered) into the high scores file
saveHighScores :: HighScore -> [String] -> IO ()
saveHighScores score scores = do
    -- Finds the high scores, inserts the new one and saves the 10 highest
    -- Due to Haskell's laziness we need to use seq to close the file from reading since we use it when we want to write to it
    length scores `seq` writeFile "score/highscores.txt" ((buildScoreString . take highScoreLimit . reverse . insert score . reverse  . buildScoreList) scores)

    where
        -- | Serializes into the High Score type
        buildScoreList :: [String] -> [HighScore]
        buildScoreList [] = []
        buildScoreList (x:xs) = HighScore (getPlayerType $ concat $ take 1 ws, read . concat $ drop 1 ws) : buildScoreList xs
            where
                ws = words x
                getPlayerType "Puck-Man" = PuckMan

        -- | Deserializes the High Score type
        buildScoreString :: [HighScore] -> String
        buildScoreString [] = []
        buildScoreString (HighScore (pt, s):xs) = show pt ++ " " ++ show s ++ "\n" ++ buildScoreString xs

-- | Loads all the animations from the file system
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

-- | Loads all the textures from the file system
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