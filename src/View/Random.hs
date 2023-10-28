module View.Random where

import System.Random (Random (randomR), RandomGen, randomRIO, StdGen)

generateSeed :: IO Int
generateSeed = randomRIO (1000000000, 9999999999)

randomElementFromList :: (RandomGen g) => [a] -> g -> (a, g)
randomElementFromList l g = (l !! index, gen)
  where
    n = length l
    (index, gen) = randomR (0, (n - 1)) g