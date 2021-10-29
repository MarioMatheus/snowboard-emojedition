module Physics.Collision where

-- import Emo8.Util.Collide (isCollide)
import Emo8.Game.Update (Update, isOutOfCanvas)

-- | Check if the object is outside the canvas.
-- |
-- | isOutOfWorld size x y.
isOutOfWorld :: Int -> Int -> Int -> Update Boolean
isOutOfWorld = isOutOfCanvas
