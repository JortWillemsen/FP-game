module View.Menu where

toggleMenu :: Bool -> Bool
toggleMenu p | p = False
             | otherwise  = True