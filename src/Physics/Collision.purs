module Physics.Collision where

import Emo8.Game.Update (Update, isOutOfCanvas)
import Emo8.Util.Collide (isCollide)

-- | Check if the object is outside the canvas.
-- |
-- | isOutOfWorld size x y.
isOutOfWorld :: Int -> Int -> Int -> Update Boolean
isOutOfWorld = isOutOfCanvas

-- | Check if the objects is collidibg.
-- |
-- | isColliding objA objB.
isColliding ::
  forall a b.
  { x :: Int, y :: Int, size :: Int | a } ->
  { x :: Int, y :: Int, size :: Int | b } ->
  Boolean
isColliding a b = isCollide a.size a.x a.y b.size b.x b.y
