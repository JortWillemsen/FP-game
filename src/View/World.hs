module View.World where

import Graphics.Gloss ()
import Model.Model ( GameState, Level, basicState )
import View.Animation ()
import View.File( loadLevel, loadHighScores, AllTextures, AllAnimations, loadAnimations, loadTextures ) 
import View.Random (generateSeed)
import System.Random (StdGen)
import Model.Constants (maxNumOfLevels, levelOne)

data WorldState = WorldState
  { gameState :: GameState,
    highScores :: [String],
    textures :: AllTextures,
    animation :: AllAnimations
  }

-- | Creates the initial world state by loading textures, animations, high scores and a random seed
createWorldState :: Level -> IO WorldState
createWorldState n =
  do
    let l = if n > maxNumOfLevels then levelOne else n    
    textures <- loadTextures
    level <- loadLevel l "level/"
    highscores <- loadHighScores
    seed <- generateSeed
    WorldState (basicState level l seed) highscores textures <$> loadAnimations

-- | Creates the initial world state for a custom level
createCustomWorldState :: WorldState -> Level -> IO WorldState
createCustomWorldState ws n =
  do
    let l = if n > maxNumOfLevels then levelOne else n 
    textures <- loadTextures
    level <- loadLevel l "level/custom/"
    highscores <- loadHighScores 
    seed <- generateSeed

    if not (null level)
      then WorldState (basicState level l seed) highscores textures <$> loadAnimations
      else return ws
