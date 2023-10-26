module Model.Score where 
import Model.Maze

type Score = (String, Int)

updateHighScores :: [Score] -> Score -> [Score]
updateHighScores (hs1@(_, s1):xs) hs2@(_, s2) | s2 >= s1  = hs2 : hs1 : xs
                                              | otherwise = hs1 : updateHighScores xs hs2

updateScore :: Position -> Maze -> Score -> (Score, Maze) 
updateScore p m s@(n, i) | hasDot m p = ((n, i + 10), removeDot m p)
                         | otherwise = (s, m)