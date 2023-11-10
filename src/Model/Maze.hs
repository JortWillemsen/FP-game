module Model.Maze where

import Data.Maybe (fromMaybe, mapMaybe)
import Model.Constants
import Model.Move (Position)
import Model.Collidable (Collidable (hitBox, collisions, name))
import Model.Ghost

data Tile
  = Floor FloorType Position (Maybe Collectable) (Maybe SpawnPoint)
  | Wall Position (Maybe WallType)
  deriving (Show, Ord, Eq)

data FloorType = Path | Trapdoor deriving (Show, Eq, Ord)

instance Collidable Tile where
  collisions (Floor Trapdoor _ _ _) = ["player"]
  collisions (Floor _ _ (Just _) _) = ["player"]
  collisions (Floor {}) = []
  collisions (Wall {}) = ["ghost", "player"]
  name (Floor Trapdoor _ _ _) = "trapdoor"

  name (Floor _ _ (Just _) _) = "collectible"
  name (Floor {}) = "floor"
  name (Wall {}) = "wall"

  hitBox (Wall p@(x, y) _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]
  hitBox (Floor Trapdoor p@(x, y) _ _) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]
  hitBox (Floor _ p@(x, y) (Just Energizer) _ ) = [p, (x, y + tileSize - 0.1), (x + tileSize - 0.1, y + tileSize - 0.1), (x + tileSize - 0.1, y)]
  hitBox (Floor _ (x, y) (Just Dot) _ ) = [(x + (tileSize / 2), y + (tileSize / 2)), (x + (tileSize / 2) + 1, y + (tileSize / 2)), (x + (tileSize / 2) + 1, y + (tileSize / 2) + 1), (x + (tileSize / 2), y + (tileSize / 2) + 1)]
  hitBox (Floor {}) = []

-- Allows for easy access
pos :: Tile -> Position
pos (Wall p _) = p
pos (Floor  _ p _ _) = p

collectable :: Tile -> Maybe Collectable
collectable (Wall {}) = Nothing
collectable (Floor _ _ x _ ) = x

type Maze = [Tile]

-- XXXXX
-- XOOOX
-- XOXOX
-- XOOOX
-- XXXXX
basicMaze :: Maze
basicMaze =
  [ Wall (0.0 * tileSize, 4.0 * tileSize) (Just (Corner Nw)),
    Wall (1.0 * tileSize, 4.0 * tileSize) (Just (Pipe H)),
    Wall (2.0 * tileSize, 4.0 * tileSize) (Just (Pipe H)),
    Wall (3.0 * tileSize, 4.0 * tileSize) (Just (Pipe H)),
    Wall (4.0 * tileSize, 4.0 * tileSize) (Just (Corner Ne)),
    Wall (0.0 * tileSize, 3.0 * tileSize) (Just (Pipe V)),
    Floor Path (1.0 * tileSize, 3.0 * tileSize) Nothing Nothing,
    Floor Path (2.0 * tileSize, 3.0 * tileSize) Nothing Nothing,
    Floor Path (3.0 * tileSize, 3.0 * tileSize) Nothing Nothing,
    Wall (4.0 * tileSize, 3.0 * tileSize) (Just (Pipe V)),
    Wall (0.0 * tileSize, 2.0 * tileSize) (Just (Pipe V)),
    Floor Path (1.0 * tileSize, 2.0 * tileSize) Nothing Nothing,
    Wall (2.0 * tileSize, 2.0 * tileSize) (Just Contained),
    Floor Path (3.0 * tileSize, 2.0 * tileSize) Nothing Nothing,
    Wall (4.0 * tileSize, 2.0 * tileSize) (Just (Pipe V)),
    Wall (0.0 * tileSize, 1.0 * tileSize) (Just (Pipe V)),
    Floor Path (1.0 * tileSize, 1.0 * tileSize) Nothing Nothing,
    Floor Path (2.0 * tileSize, 1.0 * tileSize) Nothing Nothing,
    Floor Path (3.0 * tileSize, 1.0 * tileSize) Nothing Nothing,
    Wall (4.0 * tileSize, 1.0 * tileSize) (Just (Pipe V)),
    Wall (0.0 * tileSize, 0.0 * tileSize) (Just (Corner Sw)),
    Wall (1.0 * tileSize, 0.0 * tileSize) (Just (Pipe H)),
    Wall (2.0 * tileSize, 0.0 * tileSize) (Just (Pipe H)),
    Wall (3.0 * tileSize, 0.0 * tileSize) (Just (Pipe H)),
    Wall (4.0 * tileSize, 0.0 * tileSize) (Just (Corner Se))
  ]

data SpawnPoint
  = FruitSpawn
  | PlayerSpawn
  | GhostSpawn
  | ScatterSpawn
  deriving (Eq, Show, Ord)

data CornerDirection = Nw | Ne | Sw | Se deriving (Show, Eq, Ord)

data EdgeDirection = N | E | S | W deriving (Show, Eq, Ord)

data PipeDirection = H | V deriving (Show, Eq, Ord)

data WallType
  = Corner CornerDirection
  | Edge EdgeDirection
  | Pipe PipeDirection
  | Stump EdgeDirection
  | Contained
  deriving (Show, Eq, Ord)

data Collectable
  = Dot -- 10 points
  | Energizer -- 50 points
  | Fruit
  deriving (Show, Eq, Ord)

getCollectible :: Maze -> Position -> Maybe Collectable
getCollectible m p = case findTileInMaze m p of
  (Floor _ _ c _) -> c
  _ -> Nothing

getEnergizers :: Maze -> [Tile]
getEnergizers = filter isEnergizer

isEnergizer :: Tile -> Bool
isEnergizer t = case t of
  (Wall {}) -> False
  (Floor _ _ c _) -> case c of
    Just Energizer -> True
    _ -> False 

removeCollectible :: Maze -> Position -> Maze
removeCollectible m p = case findTileInMaze m p of
  (Floor _ p (Just a) s) -> removeCollectible' m p 
  where 
    removeCollectible' (tile@(Floor t pos (Just a) s):ts) p  
      | p == pos = Floor t pos Nothing s : ts
      | otherwise = tile : removeCollectible' ts p 
    removeCollectible' (t:ts) p = t : removeCollectible' ts p 


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
      | v == '_' = Floor Path (x, y) Nothing Nothing : loadRow' vs y (x + tileSize)
      | v == 'o' = Floor Path (x, y) (Just Dot) Nothing : loadRow' vs y (x + tileSize)
      | v == 'O' = Floor Path (x, y) (Just Energizer) Nothing : loadRow' vs y (x + tileSize)
      | v == 'P' = Floor Path (x, y) Nothing (Just PlayerSpawn) : loadRow' vs y (x + tileSize)
      | v == 'G' = Floor Path (x, y) Nothing (Just GhostSpawn) : loadRow' vs y (x + tileSize)
      | v == 'F' = Floor Path (x, y) (Just Dot) (Just FruitSpawn) : loadRow' vs y (x + tileSize)
      | v == 'T' = Floor Trapdoor (x, y) Nothing Nothing : loadRow' vs y (x + tileSize)
      | v == 'S' = Floor Path (x, y) (Just Dot) (Just ScatterSpawn) : loadRow' vs y (x + tileSize)


addWallTypesToMaze :: Maze -> Maze
addWallTypesToMaze m = map (addWallTypeToTile m) m

addWallTypeToTile :: Maze -> Tile -> Tile
addWallTypeToTile m (Floor t p c s) = Floor t p c s
addWallTypeToTile m (Wall p _) = case getNeighbouringTiles m p of
  (Wall {}, Wall {}, Wall {}, Floor {}) -> Wall p $ Just (Edge S)
  (Wall {}, Wall {}, Wall {}, Wall {}) -> Wall p $ Nothing
  (Floor {}, Floor {}, Floor {}, Floor {}) -> Wall p $ Just Contained
  (Wall {}, Wall {}, Floor {}, Wall {}) -> Wall p $ Just (Edge E)
  (Wall {}, Floor {}, Wall {}, Wall {}) -> Wall p $ Just (Edge W)
  (Floor {}, Wall {}, Wall {}, Wall {}) -> Wall p $ Just (Edge N)
  (Floor {}, Floor {}, Wall {}, Wall {}) -> Wall p $ Just (Corner Nw)
  (Wall {}, Wall {}, Floor {}, Floor {}) -> Wall p $ Just (Corner Se)
  (Floor {}, Wall {}, Floor {}, Wall {}) -> Wall p $ Just (Corner Ne)
  (Wall {}, Floor {}, Wall {}, Floor {}) -> Wall p $ Just (Corner Sw)
  (Floor {}, Wall {}, Wall {}, Floor {}) -> Wall p $ Just (Pipe H)
  (Wall {}, Floor {}, Floor {}, Wall {}) -> Wall p $ Just (Pipe V)
  (Floor {}, Floor {}, Floor {}, Wall {}) -> Wall p $ Just (Stump N)
  (Wall {}, Floor {}, Floor {}, Floor {}) -> Wall p $ Just (Stump S)
  (Floor {}, Wall {}, Floor {}, Floor {}) -> Wall p $ Just (Stump E)
  (Floor {}, Floor {}, Wall {}, Floor {}) -> Wall p $ Just (Stump W)

getNeighbouringTiles :: Maze -> Position -> (Tile, Tile, Tile, Tile)
getNeighbouringTiles m (x, y) =
  ( findTileInMaze m (x, y + tileSize),
    findTileInMaze m (x - tileSize, y),
    findTileInMaze m (x + tileSize, y),
    findTileInMaze m (x, y - tileSize)
  )

-- ANDERS
getNeighbouringFloorTiles :: Maze -> Position -> [Tile]
getNeighbouringFloorTiles m (x, y) =
  getFloorTiles
    [ findTileInMaze m (x, y + tileSize),
      findTileInMaze m (x - tileSize, y),
      findTileInMaze m (x + tileSize, y),
      findTileInMaze m (x, y - tileSize)
    ]
  where
    getFloorTiles [] = []
    getFloorTiles (t : ts) = case t of
      (Floor {}) -> t : getFloorTiles ts
      _ -> getFloorTiles ts

findTileInMaze :: Maze -> Position -> Tile
findTileInMaze [] p = Wall p Nothing
findTileInMaze (t : ts) p
  | pos t == p = t
  | otherwise = findTileInMaze ts p

getMazeSize :: Maze -> Position
getMazeSize = foldr (max . pos) (0, 0)

isSpawn :: SpawnPoint -> Tile -> Bool
isSpawn spawnPoint (Floor _ _ _ (Just tileSpawn)) = tileSpawn == spawnPoint
isSpawn _ _ = False

getSpawns :: SpawnPoint -> Maze -> [Tile]
getSpawns point = filter (isSpawn point)

neighborsList :: (Tile, Tile, Tile, Tile) -> [Tile]
neighborsList (a, b, c, d) = [a, b, c, d]

floors :: Maze -> Maze
floors xs = [ x | x@(Floor {}) <- xs]