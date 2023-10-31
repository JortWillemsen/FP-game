module Model.Spawning where
import Model.Maze (getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn, ScatterSpawn), Maze, pos)
import System.Random (RandomGen, Random (randomRs))
import Model.Player (Player)
import Model.Move (Position)
import View.Random (randomElementFromList)
import Model.Ghost (Ghost)
import Data.List (nub)

randomPlayerSpawn :: (RandomGen g) => g -> Maze -> (Position, g)
randomPlayerSpawn g m = (pos spawnPoint, newGen) where
  (spawnPoint, newGen) = randomElementFromList spawns g
  spawns = getSpawns PlayerSpawn m

randomGhostSpawns :: (RandomGen g) => g -> [a] -> Maze -> ([Position], g)
randomGhostSpawns gen ghosts m = (map (spawnPositions !!) randomIndices, gen)
  where
    randomIndices = take (length ghosts) $ nub $ randomRs (0, length spawns - 1) gen
    spawns = getSpawns GhostSpawn m
    spawnPositions = map pos spawns

randomScatterSpawns :: (RandomGen g) => g -> [a] -> Maze -> ([Position], g)
randomScatterSpawns gen ghosts m = (map (spawnPositions !!) randomIndices, gen)
  where
    randomIndices = take (length ghosts) $ nub $ randomRs (0, length spawns - 1) gen
    spawns = getSpawns ScatterSpawn m
    spawnPositions = map pos spawns

