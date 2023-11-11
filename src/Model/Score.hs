{-# LANGUAGE InstanceSigs #-}
module Model.Score where

import Model.Maze
  ( getCollectible,
    pos,
    removeCollectible,
    Collectable(Energizer, Dot),
    Maze,
    Tile )
import Model.Move (Position)
import Model.Player ( PlayerType )

type Score = Int

newtype HighScore = HighScore (PlayerType, Score)

-- | Instance for Eq so we can use Ord and insert when creating the high scores
instance Eq HighScore where
    (==) :: HighScore -> HighScore -> Bool
    HighScore (_, s) == HighScore (_, s') = s == s'

    (/=) :: HighScore -> HighScore -> Bool
    h /= h' = not $ h == h'

-- | Instance for Ord so we can use insert when creating the high scores
instance Ord HighScore where
    (<) :: HighScore -> HighScore -> Bool
    HighScore (_, s) < HighScore (_, s') = s < s'
    
    (>) :: HighScore -> HighScore -> Bool
    HighScore (_, s) > HighScore (_s, s') = s > s'
    
    (<=) :: HighScore -> HighScore -> Bool
    HighScore (_, s) <= HighScore (_s, s') = s <= s'
    
    (>=) :: HighScore -> HighScore -> Bool
    HighScore (_, s) >= HighScore (_s, s') = s >= s'

-- | Removes the collectible from the maze and returns the added score
updateScore :: Tile -> Maze -> Score -> (Score, Maze)
updateScore t m s = case getCollectible m (pos t) of
  (Just Dot) -> (s + 10, removeCollectible m (pos t))
  (Just Energizer) -> (s + 50, removeCollectible m (pos t))
  Nothing -> (s, m)
