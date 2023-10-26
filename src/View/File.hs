module View.File where

import Model.Score ( Score, updateHighScores )

loadLevel :: IO [String] 
loadLevel = do
  level <- readFile "level/level.txt"
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
        buildScoreList :: [String] -> [Score]
        buildScoreList [] = []
        buildScoreList (x:xs) = (concat $ take 1 ws, read . concat $ drop 1 ws) : buildScoreList xs
            where
                ws = words x

        buildScoreString :: [Score] -> String
        buildScoreString [] = []
        buildScoreString ((n, s):xs) = n ++ " " ++ show s ++ "\n" ++ buildScoreString xs