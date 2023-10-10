data Tile = Wall Position | Floor Position


type Row = [(Tile, Maybe Collectable)]
type Maze = [Row]

-- XXX
-- X0X
-- XXX
level :: Maze
level = [
  [(Wall (0.0, 0.0)), (Wall (1.0, 0.0)), (Wall (2.0, 0.0))], 
  [(Wall (0.0, 1.0)), (Floor (1.0, 1.0)), (Wall (2.0, 1.0))], 
  [(Wall (0.0, 2.0)), (Wall (1.0, 2.0)), (Wall (2.0, 2.0))]
]