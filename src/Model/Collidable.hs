module Model.Collidable where
import Data.List (intersect)

type Position = (Float, Float)

type Name = String 

type HitBox = [Position]

-- | Every entity in the game that can be collided with needs derive from this class
class (Eq a) => Collidable a where
  hitBox :: a -> HitBox         -- Defines the size and position of the hitbox relative to the entity
  collisions :: a -> [Name]   -- Defines the entities it can collide with
  name :: a -> Name           -- Defines it's own name for other collision derivations

-- | Checks if a collision has occurred between two collidables
collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides x y = hitBox x `intersects` hitBox y && x `collidesWith` y

-- | Checks if a collision has occurred between an entity and a list of collidables
--   Returns the collidable that the entity has collided with
collidesReturn :: (Collidable a, Collidable b) => a -> [b] -> Maybe b
collidesReturn x = foldr f Nothing
  where
        f c r =
          if x `collides` c
            then Just c
            else r

-- | Checks if a collidable collides with another collidable
collidesWith :: (Collidable a, Collidable b) => a -> b -> Bool
collidesWith x y = name x `elem` collisions y || name y `elem` collisions x

-- | Checks if the collidables can collide and the second one has a specific tag
--   An implementation for this function is tryMove where we check if the collidable is specifically a wall
--   since we loop through all collidables in the maze but only want stop moving if we collide with a wall.
collidesWithTag :: (Collidable a, Collidable b) => [String] -> a -> b -> Bool
collidesWithTag tags x y = name y `elem` tags && x `collides` y

-- | Checks if a hitbox intersects with another hitbox
intersects :: HitBox -> HitBox -> Bool
intersects hitbox s = any (`inSquare` s) hitbox || any (`inSquare` hitbox) s where
  -- | Checks if a position is inside a hitbox
  inSquare :: Position -> HitBox -> Bool
  inSquare _ [] = False
  inSquare (x, y) [(bLX, bLY), _, (tRX, tRY), _] = x > bLX && y > bLY && x <= tRX && y <= tRY