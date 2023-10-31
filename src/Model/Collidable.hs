module Model.Collidable where
import Data.List (intersect)

type Position = (Float, Float)

type HitBox = [Position]

class (Eq a) => Collidable a where
  hitBox :: a -> HitBox
  collisions :: a -> [String]
  name :: a -> String

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y = hitBox x `intersects` hitBox y && x `collidable` y

collidable :: (Collidable a, Collidable b) => a -> b -> Bool
collidable x y = (name x `elem` collisions y) || (name y `elem` collisions x)

intersects :: HitBox -> HitBox -> Bool
intersects hitbox s = any (`inSquare` s) hitbox where
  inSquare :: Position -> [Position] -> Bool
  inSquare _ [] = False
  inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY