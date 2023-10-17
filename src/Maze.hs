module Maze where

import Data.Maybe (fromMaybe, mapMaybe)
import Move

data Tile
  = Wall Position (Maybe WallType)
  | Floor Position
  deriving (Show)

tileSize :: Float
tileSize = 16.0

-- Allows for easy access
pos :: Tile -> Position
pos (Wall p _) = p
pos (Floor p) = p

type Maze = [Tile]

data CornerDirection = Nw | Ne | Sw | Se deriving (Show)

data EdgeDirection = N | E | S | W deriving (Show)

data PipeDirection = H | V deriving (Show)

data WallType
  = Corner CornerDirection
  | Edge EdgeDirection
  | Pipe PipeDirection
  | Stump EdgeDirection
  | Contained
  deriving (Show)

data Collectable
  = Dot -- 10 points
  | Energizer -- 50 points
  | Fruit
  deriving (Show)

loadMaze :: [String] -> Maze
loadMaze rs = addWallTypesToMaze $ loadMaze' (reverse rs) 0
  where
    loadMaze' [] _ = []
    loadMaze' (r : rs) y = loadRow r y ++ loadMaze' rs (y + tileSize)

-- Takes a line of input and a y value in the maze and delivers a row with correct positions
loadRow :: [Char] -> Float -> [Tile]
loadRow vs y = loadRow' vs y 0
  where
    loadRow' [] y x = []
    loadRow' (v : vs) y x
      | v == 'X' = Wall (x, y) Nothing : loadRow' vs y (x + tileSize)
      | v == 'O' = Floor (x, y) : loadRow' vs y (x + tileSize)

addWallTypesToMaze :: Maze -> Maze
addWallTypesToMaze m = map (addWallTypeToTile m) m

addWallTypeToTile :: Maze -> Tile -> Tile
addWallTypeToTile m (Floor p) = Floor p
addWallTypeToTile m (Wall p _) = case getNeighbouringTiles m p of
  (Wall _ _, Wall _ _, Wall _ _, Floor _) -> Wall p $ Just (Edge S)
  (Wall _ _, Wall _ _, Wall _ _, Wall _ _) -> Wall p $ Nothing
  (Floor _, Floor _, Floor _, Floor _) -> Wall p $ Just Contained
  (Wall _ _, Wall _ _, Floor _, Wall _ _) -> Wall p $ Just (Edge E)
  (Wall _ _, Floor _, Wall _ _, Wall _ _) -> Wall p $ Just (Edge W)
  (Floor _, Wall _ _, Wall _ _, Wall _ _) -> Wall p $ Just (Edge N)
  (Floor _, Floor _, Wall _ _, Wall _ _) -> Wall p $ Just (Corner Nw)
  (Wall _ _, Wall _ _, Floor _, Floor _) -> Wall p $ Just (Corner Se)
  (Floor _, Wall _ _, Floor _, Wall _ _) -> Wall p $ Just (Corner Ne)
  (Wall _ _, Floor _, Wall _ _, Floor _) -> Wall p $ Just (Corner Sw)
  (Floor _, Wall _ _, Wall _ _, Floor _) -> Wall p $ Just (Pipe H)
  (Wall _ _, Floor _, Floor _, Wall _ _) -> Wall p $ Just (Pipe V)
  (Floor _, Floor _, Floor _, Wall _ _) -> Wall p $ Just (Stump N)
  (Wall _ _, Floor _, Floor _, Floor _) -> Wall p $ Just (Stump S)
  (Floor _, Wall _ _, Floor _, Floor _) -> Wall p $ Just (Stump E)
  (Floor _, Floor _, Wall _ _, Floor _) -> Wall p $ Just (Stump W)

getNeighbouringTiles :: Maze -> Position -> (Tile, Tile, Tile, Tile)
getNeighbouringTiles m (x, y) =
  ( findTileInMaze m (x, y + tileSize),
    findTileInMaze m (x - tileSize, y),
    findTileInMaze m (x + tileSize, y),
    findTileInMaze m (x, y - tileSize)
  )

findTileInMaze :: Maze -> Position -> Tile
findTileInMaze [] p = Wall p Nothing
findTileInMaze (t : ts) p
  | pos t == p = t
  | otherwise = findTileInMaze ts p