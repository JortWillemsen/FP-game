module Maze where

import Moveable

data Tile
  = Wall Position (Maybe WallType)
  | Floor Position
  deriving (Show)

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

type Row = [Tile]

type Maze = [Row]

-- XXXXX
-- XOOOX
-- XOXOX
-- XOOOX
-- XXXXX
basicLevel :: Maze
basicLevel =
  [ [Wall (0.0, 0.0) (Just (Corner Nw)), Wall (1.0, 0.0) (Just (Pipe H)), Wall (2.0, 0.0) (Just (Pipe H)), Wall (3.0, 0.0) (Just (Pipe H)), Wall (4.0, 0.0) (Just (Corner Ne))],
    [Wall (0.0, 1.0) (Just (Pipe V)), Floor (1.0, 1.0), Floor (2.0, 1.0), Floor (3.0, 1.0), Wall (4.0, 1.0) (Just (Pipe V))],
    [Wall (0.0, 2.0) (Just (Pipe V)), Floor (1.0, 2.0), Wall (2.0, 2.0) (Just Contained), Floor (3.0, 2.0), Wall (4.0, 2.0) (Just (Pipe V))],
    [Wall (0.0, 3.0) (Just (Pipe V)), Floor (1.0, 3.0), Floor (2.0, 3.0), Floor (3.0, 3.0), Wall (4.0, 3.0) (Just (Pipe V))],
    [Wall (0.0, 4.0) (Just (Corner Sw)), Wall (1.0, 4.0) (Just (Pipe H)), Wall (2.0, 4.0) (Just (Pipe H)), Wall (3.0, 4.0) (Just (Pipe H)), Wall (4.0, 4.0) (Just (Corner Se))]
  ]

loadLevel :: [String] -> Maze
loadLevel rs = loadLevel' (reverse rs) 0
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


-- Contains the file path to the BMP for every wall type
bitmaps :: [(WallType, String)]
bitmaps = [
    (Corner Nw, "./Sprites/walls/wall_corner_tl.bmp"),
    (Corner Ne, "./Sprites/walls/wall_corner_tr.bmp"),
    (Corner Sw, "./Sprites/walls/wall_corner_bl.bmp"),
    (Corner Se, "./Sprites/walls/wall_corner_br.bmp"),

    (Edge N, "./Sprites/walls/wall_edge_tp.bmp"),
    (Edge E, "./Sprites/walls/wall_edge_r.bmp"),
    (Edge S, "./Sprites/walls/wall_edge_btm.bmp"),
    (Edge W, "./Sprites/walls/wall_edge_l.bmp"),

    (Pipe H, "./Sprites/walls/wall_contained.bmp"),
    (Pipe V, "./Sprites/walls/wall_contained.bmp"),
    
    (Stump N, "./Sprites/walls/wall_stump_tp.bmp"),
    (Stump E, "./Sprites/walls/wall_stump_r.bmp"),
    (Stump S, "./Sprites/walls/wall_stump_btm.bmp"),
    (Stump W, "./Sprites/walls/wall_stump_l.bmp"),

    (Contained, "./Sprites/walls/wall_contained.bmp"),
  ]

