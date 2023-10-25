module Ghost where
import Maze ( Maze, getNeighbouringTiles, Tile(Wall, Floor), getNeighbouringFloorTiles, tileSize )
import Move ( Position, Direction )
import Player (Player (Player))

data Ghost = Ghost {ghostType :: GhostType
                  , ghpos :: Position
                  , ghdir :: Direction}

data GhostType = Blinky
                | Pinky
                | Inky
                | Clyde

collidesWithPlayer :: Player -> Ghost -> Bool
collidesWithPlayer (Player _ posP _ _) (Ghost _ posG _) = hitbox posP `intersect` hitbox posG
    where
        hitbox :: Position -> [Position] -- TODO zou dit nog anders kunnen?
        hitbox p@(x, y) = [p, (x, y+tileSize-0.1), (x+tileSize-0.1, y+tileSize-0.1), (x+tileSize-0.1, y)]

        intersect :: [Position] -> [Position] -> Bool
        intersect hitbox s = any (`inSquare` s) hitbox
            where
                inSquare :: Position -> [Position] -> Bool
                inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY


data Node = Node { node :: Int, pos :: Position }
data Edge = Edge { startPos :: Position, endPos :: Position, weight :: Int}

createNodeList :: Maze -> [Node]
createNodeList [] = []
createNodeList m = createNodeList' m m 1
  where
    createNodeList' (t:ts) m' n = case t of
        (Floor pos _ _) -> if length (getNeighbouringFloorTiles m' pos) > 2
                            then Node { node = n, pos = pos } : createNodeList' ts m' (n+1)
                            else createNodeList' ts m' n
        _ -> createNodeList' ts m' n

createEdgeList :: [Node] -> [Edge]
createEdgeList ns = createEdgeList' (combinations [i | Node i _ <- ns]) ns
    where
        createEdgeList' cs ((Node i _):ns) = createEdges ls
            where
                createEdges (c:cs) = undefined
                ls = filter (\l -> i `elem` l) cs

        combinations :: [Int] -> [[Int]]
        combinations ints = combinations' 2 ints
            where
                combinations' 0 _ = [[]]
                combinations' _ [] = []
                combinations' i (int:ints) = map (int:) (combinations' (i-1) ints) ++ combinations' i ints

-- moveAlgorithm :: Ghost -> Player -> Maze -> Ghost
-- moveAlgorithm (Blinky sP d) (PuckMan eP _ _ ) m = undefined



