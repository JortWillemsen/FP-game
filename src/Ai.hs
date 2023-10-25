{-# LANGUAGE InstanceSigs #-}

module Ai where

import Data.List
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe, mapMaybe)
import Maze
import Move (Position)
import Debug.Trace (trace)

data Distance = Distance Int | Infinite deriving (Show, Eq)

instance Ord Distance where
  (<=) :: Distance -> Distance -> Bool
  Infinite <= Infinite = True
  Infinite <= Distance x = False
  Distance x <= Infinite = True
  Distance x <= Distance y = x <= y

data Node = Node Position Distance deriving (Show)

instance Ord Node where
  (>) n1 n2 = Ai.pos n1 > Ai.pos n2
  (<) n1 n2 = Ai.pos n1 < Ai.pos n2
  (>=) n1 n2 = Ai.pos n1 >= Ai.pos n2
  (<=) n1 n2 = Ai.pos n1 <= Ai.pos n2

instance Eq Node where
  (==) n1 n2 = Ai.pos n1 == Ai.pos n2

pos :: Node -> Position
pos (Node p _) = p

posIn :: Position -> [Node] -> Bool
posIn p nodes = foldr f False nodes where
  f (Node p' _) r = p == p' || r

data Graph = Graph [(Node, [Position])]

instance Show Graph where
  show (Graph xs) = "Graph: " ++ show xs

data Dijkstra = State {unvisited :: [Node]}

initialGraph :: Maze -> Graph
initialGraph m = Graph $ buildNodes m

buildNodes :: Maze -> [(Node, [Position])]
buildNodes maze =
  buildNodes' maze maze 
  where
    buildNodes' _ [] = []
    buildNodes' maze current@(x:xs) = let node = findNode x
        in case node of
        Nothing -> buildNodes' maze xs
        Just n -> (n, findConnections n) : buildNodes' maze xs
    findNode :: Tile -> Maybe Node
    findNode (Floor p _ _) = Just (Node p Infinite)
    findNode (Wall {}) = Nothing
    findConnections :: Node -> [Position]
    findConnections n = floors $ neighborsToList $ getNeighbouringTiles maze $ Ai.pos n

addDistance :: Graph -> Position -> Position -> [Node]
addDistance g s e =
  let start = Node s (Distance 0)
   in addDistance' g start e []
  where
    addDistance' :: Graph -> Node -> Position -> [Node] -> [Node]
    addDistance' g@(Graph nodes) c@(Node p d) e xs =
      let posses = lookup c nodes
          neighbors = case posses of
            Just nbs -> filter (\x -> not $ x `posIn` xs) nbs
            otherwise -> []
        in if Ai.pos c == e
            then (c : xs)
            else concatMap (\x -> addDistance' g x e (c : xs)) $ map (`visit` d) neighbors

visit :: Position -> Distance -> Node
visit p Infinite = (Node p Infinite)
visit p (Distance x) = (Node p (Distance (x + 1)))