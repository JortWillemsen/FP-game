module Model.Spawning where
import Model.Maze (getSpawns, SpawnPoint (PlayerSpawn, GhostSpawn, ScatterSpawn), Maze, pos)
import System.Random (RandomGen, Random (randomRs))
import Model.Player (Player)
import Model.Move (Position)
import View.Random (randomElementFromList, randomIndexesFromList)
import Model.Ghost (Ghost)
import Data.List (nub)

-- | Calculates a random spawn point for the player based on all possible spawn points
randomPlayerSpawn :: (RandomGen g) => g -> Maze -> (Position, g)
randomPlayerSpawn g m = (pos spawnPoint, newGen) where
  (spawnPoint, newGen) = randomElementFromList spawns g
  spawns = getSpawns PlayerSpawn m

-- | Assigns a random spawn point for each ghost based on all possible spawn points
randomSpawns :: (RandomGen g) => g -> SpawnPoint -> [a] -> Maze -> ([Position], g)
randomSpawns gen sp ghosts m = (map (spawnPositions !!) $ randomIndexesFromList gen (length ghosts) (length spawns), gen)
  where
    spawns = getSpawns sp m
    spawnPositions = map pos spawns
