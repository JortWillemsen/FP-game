{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
module View.File where

import Model.Score ( Score, updateHighScores, HighScore )
import Model.Model ( Level )
import Control.Monad (when)
import Model.Player (PlayerType (PuckMan))


loadLevel :: Level -> IO [String]
loadLevel i = do
    level <- readFile ("level/" ++ show i ++ ".txt")
--   let fileName = "level/level/" ++ show i ++ ".txt"
--       level    = if doesFileExist fileName
--                     then readFile fileName
--                     else readFile "level/level/1.txt"
    return $ lines level

loadCustomLevel :: Level -> IO [String]
loadCustomLevel i = do
    level <- readFile ("level/custom/" ++ show i ++ ".txt")
--   let fileName = "level/level/" ++ show i ++ ".txt"
--       level    = if doesFileExist fileName
--                     then readFile fileName
--                     else readFile "level/level/1.txt"
    return $ lines level

loadHighScores :: IO [String]
loadHighScores = do
    scores <- readFile "score/highscores.txt"
    return $ lines scores

saveHighScores :: (PlayerType, Score) -> [String] -> IO ()
saveHighScores score scores = do
    length scores `seq` writeFile "score/highscores.txt" (buildScoreString $ take 10 $ updateHighScores (buildScoreList scores) score)

    where
        buildScoreList :: [String] -> [HighScore]
        buildScoreList [] = []
        buildScoreList (x:xs) = (getPlayerType $ concat $ take 1 ws, read . concat $ drop 1 ws) : buildScoreList xs
            where
                ws = words x
                getPlayerType "Puck-Man" = PuckMan

        buildScoreString :: [HighScore] -> String
        buildScoreString [] = []
        buildScoreString ((pt, s):xs) = show pt ++ " " ++ show s ++ "\n" ++ buildScoreString xs
