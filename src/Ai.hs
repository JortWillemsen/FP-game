{-# LANGUAGE InstanceSigs #-}

module Ai where

import Data.Map (Map, fromList)
import Data.List
import Data.Maybe (fromMaybe, mapMaybe)
import Maze (Maze, Tile (Floor, Wall), floors, getNeighbouringTiles, neighborsToList, pos)
import Move (Position)

data Distance = Distance Int | Infinite deriving (Show, Eq)

instance Ord Distance where
  (<=) :: Distance -> Distance -> Bool
  Infinite <= Infinite = True
  Infinite <= Distance x = False
  Distance x <= Infinite = True
  Distance x <= Distance y = x <= y

data Node = Node Position Distance

instance Ord Node where
  (>) n1 n2 = Ai.pos n1 > Ai.pos n2
  (<) n1 n2 = Ai.pos n1 < Ai.pos n2
  (>=) n1 n2 = Ai.pos n1 >= Ai.pos n2
  (<=) n1 n2 = Ai.pos n1 <= Ai.pos n2

instance Eq Node where
  (==) n1 n2 = Ai.pos n1 == Ai.pos n2

pos :: Node -> Position
pos (Node p _) = p

data Graph = Graph
  { -- A list of nodes with it's neighbors
    nodes :: [(Node, [Position])]
  }

data Dijkstra = State {unvisited :: [Node]}

initialGraph :: Maze -> Graph
initialGraph m = Graph {nodes = buildNodes m}

buildNodes :: Maze -> [(Node, [Position])]
buildNodes maze@(x : xs) =
  let node = findNode x
   in case node of
        Nothing -> buildNodes xs
        Just n -> (n, findConnections n) : buildNodes xs
  where
    findNode :: Tile -> Maybe Node
    findNode (Floor p _ _) = Just (Node p Infinite)
    findNode (Wall {}) = Nothing
    findConnections :: Node -> [Position]
    findConnections n = map Maze.pos $ floors $ neighborsToList $ getNeighbouringTiles maze $ Ai.pos n

addDistance :: Graph -> Position -> Position -> [Node]
addDistance g s e =
  let start = Node s (Distance 0)
   in addDistance' g start e []
  where
    addDistance' :: Graph -> Node -> Position -> [Node] -> [Node]
    addDistance' g c@(Node p d) e xs = 
      let neighbors = lookup c (nodes g) in 
        case neighbors of
          Just ns -> map (\x -> visit x d) ns
          otherwise -> [] 
    visit :: Position -> Distance -> Node
    visit p Infinite = (Node p Infinite)
    visit p (Distance x) = (Node p (Distance (x + 1)))