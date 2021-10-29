module Main where

import Prelude
import Data.Array (cons, filterA, length)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Element.Hud (getScoreHud)
import Element.Obstacle (Obstacle, createRandomObstacle, movementObstacle)
import Element.Player (Player, updatePlayer)
import Emo8 (emo8)
import Emo8.Data.Color as C
import Emo8.Data.Emoji as E
import Emo8.Game (class Game)
import Emo8.Game.Draw (cls, emo, emor, emor')
import Emo8.Game.Update (Update, getCanvasSize)
import Emo8.Type (Size)
import Emo8.Util.Collide (sinkCanvas)
import Physics.Collision (isOutOfWorld)

-- import Debug.Trace (trace)
type PlayStateData
  = { player :: Player
    , obstacles :: Array Obstacle
    , score :: Int
    , obstacleSurpassCount :: Int
    , isObstacleSurpassEnable :: Boolean
    }

data GameState
  = PlayState PlayStateData

instance gameState :: Game GameState where
  update input (PlayState state) = do
    updatedState <- system state
    pure
      $ PlayState updatedState
    where
    system :: PlayStateData -> Update PlayStateData
    system =
      canvasCollision
        <=< removeOutObstacles
        <<< updateObstacles
        <<< updatePlayer'
        <<< createObstacles
        <<< updateScore
        <<< updateSurpassCount

    updateScore :: PlayStateData -> PlayStateData
    updateScore s =
      s
        { score =
          s.score
            + case mod s.obstacleSurpassCount 5, s.isObstacleSurpassEnable of
                0, true -> 1
                _, _ -> 0
        }

    updateSurpassCount :: PlayStateData -> PlayStateData
    updateSurpassCount s =
      s
        { obstacleSurpassCount = s.obstacleSurpassCount + 1 - length s.obstacles
        , isObstacleSurpassEnable =
          case length s.obstacles of
            0 -> true
            _ -> false
        }

    updateObstacles :: PlayStateData -> PlayStateData
    updateObstacles s = s { obstacles = movementObstacle <$> s.obstacles }

    createObstacles :: PlayStateData -> PlayStateData
    createObstacles s =
      s
        { obstacles =
          case length s.obstacles of
            0 -> cons (createRandomObstacle (1.0 + 0.2 * toNumber s.score)) s.obstacles
            _ -> s.obstacles
        }

    updatePlayer' :: PlayStateData -> PlayStateData
    updatePlayer' s = s { player = updatePlayer input s.player }

    removeOutObstacles :: PlayStateData -> Update PlayStateData
    removeOutObstacles s = do
      obstacles' <- filterOutObstacles s.obstacles
      pure $ s { obstacles = obstacles' }
      where
      filterOutObstacles :: Array Obstacle -> Update (Array Obstacle)
      filterOutObstacles = filterA (pure <<< not <=< isObstacleOut)

      isObstacleOut :: Obstacle -> Update Boolean
      isObstacleOut { size, x, y } = isOutOfWorld size x y

    canvasCollision :: PlayStateData -> Update PlayStateData
    canvasCollision s = do
      r <- getCanvasSize
      pure case sinkCanvas r emoSize s.player.x s.player.y of
        Just sink -> s { player { x = s.player.x - sink.x, y = s.player.y - sink.y } }
        Nothing -> s
  draw (PlayState state) = do
    cls C.snow
    emor' state.player.rotation E.snowboarder emoSize state.player.x state.player.y
    traverse_ drawObstacles state.obstacles
    traverse_ drawHud $ getScoreHud state.score
    where
    drawObstacles o = emor o.rotation o.emoji o.size o.x o.y

    drawHud e = emo e.emoji e.size e.x e.y
  sound _ = pure unit

emoSize :: Size
emoSize = 32

initialState :: GameState
initialState =
  PlayState
    { player:
        { x: 100
        , y: 150
        , rotation: 0
        }
    , obstacles: []
    , score: 0
    , obstacleSurpassCount: -1
    , isObstacleSurpassEnable: false
    }

main :: Effect Unit
main = do
  emo8 initialState conf
  where
  conf =
    { canvasSize:
        { width: 320
        , height: 240
        }
    , retina: true
    }
