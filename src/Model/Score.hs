module Model.Score where 
import Model.Maze
import Model.Move (Position)

type Score = Int
type HighScore = (String, Int)

updateHighScores :: [HighScore] -> HighScore -> [HighScore]
updateHighScores (hs1@(_, s1):xs) hs2@(_, s2) | s2 >= s1  = hs2 : hs1 : xs
                                              | otherwise = hs1 : updateHighScores xs hs2

updateScore :: Tile -> Maze -> Score -> (Score, Maze) 
updateScore t m s = case getCollectible m (pos t) of
    (Just Dot) -> (s + 10, removeCollectible m (pos t))
    (Just Energizer) -> (s + 50, removeCollectible m (pos t))
    Nothing -> (s, m)
