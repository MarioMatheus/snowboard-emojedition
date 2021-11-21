module Main where

import Prelude
import Effect (Effect)
import Data.Array (cons, filterA, length, any)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Emo8 (emo8)
import Emo8.Data.Color as C
import Emo8.Data.Emoji as E
import Emo8.Game (class Game)
import Emo8.Game.Draw (cls, emo, emor, emor')
import Emo8.Game.Update (Update, getCanvasSize)
import Emo8.Util.Collide (sinkCanvas)
import Element.Hud (getScoreHud)
import Element.Obstacle (Obstacle, createRandomObstacle, movementObstacle)
import Element.Player (Player, updatePlayer, movementFall)
import Physics.Collision (isOutOfWorld, isColliding)

-- import Debug.Trace (trace)
type PlayStateData
  = { player :: Player
    , obstacles :: Array Obstacle
    , score :: Int
    , obstacleSurpassCount :: Int
    , isObstacleSurpassEnable :: Boolean
    , nextState :: Maybe GameStage
    }

type GameOverData
  = { player :: Player
    , score :: Int
    , canRestart :: Boolean
    , nextState :: Maybe GameStage
    }

data GameState
  = PlayState PlayStateData
  | GameOverState GameOverData

data GameStage
  = Play
  | GameOver

instance gameState :: Game GameState where
  update input = case _ of
    GameOverState state ->
      pure case updatedState.nextState of
        Just (Play) -> initialState
        _ -> GameOverState updatedState
      where
      updatedState :: GameOverData
      updatedState = system state

      system :: GameOverData -> GameOverData
      system =
        restartGame
          <<< movementFallPlayer

      restartGame :: GameOverData -> GameOverData
      restartGame s = case s.canRestart, input.isUp of
        true, true -> s { nextState = Just Play }
        _, _ -> s { nextState = Nothing }

      movementFallPlayer :: GameOverData -> GameOverData
      movementFallPlayer s
        | s.player.y > 20 = s { player = movementFall s.player }
        | otherwise = s { canRestart = true }
    PlayState state -> do
      updatedState <- system state
      pure case updatedState.nextState of
        Just (GameOver) ->
          GameOverState
            { player: updatedState.player
            , score: updatedState.score
            , canRestart: false
            , nextState: Nothing
            }
        _ -> PlayState updatedState
      where
      system :: PlayStateData -> Update PlayStateData
      system =
        canvasCollision
          <=< removeOutObstacles
          <<< obstacleCollision
          <<< updateObstacles
          <<< updatePlayer'
          <<< createObstacles
          <<< updateScore
          <<< updateSurpassCount

      updateScore :: PlayStateData -> PlayStateData
      updateScore s =
        s
          { score = s.score + scoreIncrement s.isObstacleSurpassEnable s.obstacleSurpassCount
          }
        where
        scoreIncrement :: Boolean -> Int -> Int
        scoreIncrement false _ = 0

        scoreIncrement true obstacleCount
          | mod obstacleCount 5 == 0 = 1
          | otherwise = 0

      updateSurpassCount :: PlayStateData -> PlayStateData
      updateSurpassCount s =
        s
          { obstacleSurpassCount = s.obstacleSurpassCount + 1 - length s.obstacles
          , isObstacleSurpassEnable = (length s.obstacles) == 0
          }

      updateObstacles :: PlayStateData -> PlayStateData
      updateObstacles s = s { obstacles = movementObstacle <$> s.obstacles }

      createObstacles :: PlayStateData -> PlayStateData
      createObstacles s
        | length s.obstacles == 0 = s { obstacles = addObstacleWithSpeed (1.0 + 0.2 * toNumber s.score) }
          where
          addObstacleWithSpeed :: Number -> Array Obstacle
          addObstacleWithSpeed speed = cons (createRandomObstacle speed) s.obstacles
        | otherwise = s

      updatePlayer' :: PlayStateData -> PlayStateData
      updatePlayer' s = s { player = updatePlayer input s.player }

      obstacleCollision :: PlayStateData -> PlayStateData
      obstacleCollision s = s { nextState = next }
        where
        next :: Maybe GameStage
        next
          | any isColliding' s.obstacles = Just GameOver
          | otherwise = Nothing

        isColliding' :: Obstacle -> Boolean
        isColliding' = isColliding s.player

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
        pure case sinkCanvas r s.player.size s.player.x s.player.y of
          Just sink -> s { player { x = s.player.x - sink.x, y = s.player.y - sink.y } }
          Nothing -> s
  draw (PlayState state) = do
    cls C.snow
    traverse_ drawObstacles state.obstacles
    traverse_ drawHud $ getScoreHud state.score
    emor' state.player.rotation E.snowboarder state.player.size state.player.x state.player.y
    where
    drawObstacles o = emor o.rotation o.emoji o.size o.x o.y

    drawHud e = emo e.emoji e.size e.x e.y
  draw (GameOverState state) = do
    cls C.black
    emor' state.player.rotation E.snowboarder state.player.size state.player.x state.player.y
    emor' 0 emojiReact 36 142 102
    emor' 0 E.backhandIndexPointingRight 8 162 instructionEmojiY
    emor' 0 E.upwardsButton 8 150 instructionEmojiY
    traverse_ drawHud $ getScoreHud state.score
    where
    drawHud e = emo e.emoji e.size e.x e.y

    emojiReact :: E.Emoji
    emojiReact
      | state.canRestart = E.expressionlessFace
      | otherwise = E.faceWithHandOverMouth

    instructionEmojiY :: Int
    instructionEmojiY
      | state.canRestart = 96
      | otherwise = -10
  sound _ = pure unit

initialState :: GameState
initialState =
  PlayState
    { player:
        { x: 100
        , y: 150
        , rotation: 0
        , size: 32
        }
    , obstacles: []
    , score: 0
    , obstacleSurpassCount: -1
    , isObstacleSurpassEnable: false
    , nextState: Nothing
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
