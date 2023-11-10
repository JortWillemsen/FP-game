module Model.Collidable where
import Data.List (intersect)

type Position = (Float, Float)

type HitBox = [Position]

class (Eq a) => Collidable a where
  hitBox :: a -> HitBox
  collisions :: a -> [String]
  name :: a -> String

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y = hitBox x `intersects` hitBox y && x `collidable` y where
  collidable :: (Collidable a, Collidable b) => a -> b -> Bool
  collidable x y = (name x `elem` collisions y) || (name y `elem` collisions x)


collidesReturn :: (Collidable a, Collidable b) => a -> [b] -> Maybe b
collidesReturn x = foldr f Nothing
  where
        f c r =
          if x `collides` c
            then Just c
            else r

collidesWith :: (Collidable a, Collidable b) => a -> b -> [String] -> Bool
collidesWith x y tags = x `collides` y && name y `elem` tags

intersects :: HitBox -> HitBox -> Bool
intersects hitbox s = any (`inSquare` s) hitbox || any (`inSquare` hitbox) s where
  inSquare :: Position -> [Position] -> Bool
  inSquare _ [] = False
  inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY