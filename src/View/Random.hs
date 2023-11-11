module View.Random where

import System.Random (Random (randomR, randomRs), RandomGen, randomRIO, StdGen)
import Data.List (nub)

-- | Generates a 10 digit seed used in the deterministic randomness
generateSeed :: IO Int
generateSeed = randomRIO (1000000000, 9999999999)

-- | Creates m random numbers and takes n from that
-- | We can use when we have 10 possible spawn positions but only 5 ghosts. 
-- | This will ensure that we randomly select 5 of the spawn points and assign them to random ghosts
randomIndexesFromList :: (Eq a, Random a, RandomGen g, Num a) => g -> Int -> a -> [a]
randomIndexesFromList gen n m = take n $ nub $ randomRs (0, m - 1) gen

-- | Grabs a random element from a list
randomElementFromList :: (RandomGen g) => [a] -> g -> (a, g)
randomElementFromList l g = (l !! index, gen)
  where
    n = length l
    (index, gen) = randomR (0, n - 1) g