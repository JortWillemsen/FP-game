module Model.Collidable where

type Position = (Float, Float)

type HitBox = [Position]

class Collidable a where
  hitBox :: a -> HitBox

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y = hitBox x `intersects` hitBox y

intersects :: HitBox -> HitBox -> Bool
intersects hitbox s = any (`inSquare` s) hitbox where
  inSquare :: Position -> [Position] -> Bool
  inSquare _ [] = False
  inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY