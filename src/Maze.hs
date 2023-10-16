module Maze where

import Move

data Tile
  = Wall Position (Maybe WallType)
  | Floor Position
  deriving (Show)

type Row = [Tile]

type Maze = [Row]

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

-- XXXXX
-- XOOOX
-- XOXOX
-- XOOOX
-- XXXXX
basicMaze :: Maze
basicMaze =
  [ [Wall (0.0, 4.0) (Just (Corner Nw)), Wall (1.0, 4.0) (Just (Pipe H)), Wall (2.0, 4.0) (Just (Pipe H)), Wall (3.0, 4.0) (Just (Pipe H)), Wall (4.0, 4.0) (Just (Corner Ne))],
    [Wall (0.0, 3.0) (Just (Pipe V)), Floor (1.0, 3.0), Floor (2.0, 3.0), Floor (3.0, 3.0), Wall (4.0, 3.0) (Just (Pipe V))],
    [Wall (0.0, 2.0) (Just (Pipe V)), Floor (1.0, 2.0), Wall (2.0, 2.0) (Just Contained), Floor (3.0, 2.0), Wall (4.0, 2.0) (Just (Pipe V))],
    [Wall (0.0, 1.0) (Just (Pipe V)), Floor (1.0, 1.0), Floor (2.0, 1.0), Floor (3.0, 1.0), Wall (4.0, 1.0) (Just (Pipe V))],
    [Wall (0.0, 0.0) (Just (Corner Sw)), Wall (1.0, 0.0) (Just (Pipe H)), Wall (2.0, 0.0) (Just (Pipe H)), Wall (3.0, 0.0) (Just (Pipe H)), Wall (4.0, 0.0) (Just (Corner Se))]
  ]

loadMaze :: [String] -> Maze
loadMaze rs = loadLevel' (reverse rs) 0
  where
    loadLevel' [] _ = []
    loadLevel' (r : rs) y = loadRow r y : loadLevel' rs (y + 1)

-- Takes a line of input and a y value in the maze and delivers a row with correct positions
loadRow :: [Char] -> Float -> Row
loadRow vs y = loadRow' vs y 0
  where
    loadRow' [] y x = []
    loadRow' (v : vs) y x
      | v == 'X' = Wall (x, y) Nothing : loadRow' vs y (x + 1)
      | v == 'O' = Floor (x, y) : loadRow' vs y (x + 1)
