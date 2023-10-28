module View.File where

import Model.Score ( Score, updateHighScores, HighScore )
import Model.Model

loadLevel :: Level -> IO [String]
loadLevel i = do
    level <- readFile ("level/" ++ show i ++ ".txt")
--   let fileName = "level/level/" ++ show i ++ ".txt"
--       level    = if doesFileExist fileName
--                     then readFile fileName
--                     else readFile "level/level/1.txt"
    return $ lines level


loadHighScores :: IO [String]
loadHighScores = do
    scores <- readFile "score/highscores.txt"
    return $ lines scores

saveHighScores :: (String, Int) -> IO ()
saveHighScores score = do
    scores <- loadHighScores
    if length scores > 1 -- Ok als dit weg is doet ie raar?
        then writeFile "score/highscores.txt" (buildScoreString $ take 10 $ updateHighScores (buildScoreList scores) score)
        else writeFile "score/highscores.txt" (buildScoreString [score])

    where
        buildScoreList :: [String] -> [HighScore]
        buildScoreList [] = []
        buildScoreList (x:xs) = (concat $ take 1 ws, read . concat $ drop 1 ws) : buildScoreList xs
            where
                ws = words x

        buildScoreString :: [HighScore] -> String
        buildScoreString [] = []
        buildScoreString ((n, s):xs) = n ++ " " ++ show s ++ "\n" ++ buildScoreString xs