module Model.Score where 
import Model.Maze
import Model.Move (Position)

type Score = Int
type HighScore = (String, Int)

updateHighScores :: [HighScore] -> HighScore -> [HighScore]
updateHighScores (hs1@(_, s1):xs) hs2@(_, s2) | s2 >= s1  = hs2 : hs1 : xs
                                              | otherwise = hs1 : updateHighScores xs hs2

updateScore :: Position -> Maze -> Score -> (Score, Maze) 
updateScore p m s = case getCollectible m p of
    (Just Dot) -> (s + 10, removeCollectible m p)
    (Just Energizer) -> (s + 50, removeCollectible m p)
    Nothing -> (s, m)