module Element.Obstacle where

import Prelude
import Data.Array ((!!))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Emo8.Data.Emoji as E
import Math (round)

type Obstacle
  = { emoji :: E.Emoji
    , x :: Int
    , y :: Int
    , size :: Int
    , rotation :: Int
    , speed :: Number
    }

obstacleEmojis :: Array E.Emoji
obstacleEmojis =
  [ E.rock
  , E.wood
  , E.snowmanWithoutSnow
  , E.pool8Ball
  , E.teddyBear
  , E.beachWithUmbrella
  , E.skateboard
  , E.smallAirplane
  , E.flyingSaucer
  , E.pizza
  ]

movementObstacle :: Obstacle -> Obstacle
movementObstacle o = o { x = o.x - xSpeed, y = o.y + ySpeed }
  where
  xSpeed :: Int
  xSpeed = roundSpeed (o.speed * 2.0)

  ySpeed :: Int
  ySpeed = roundSpeed o.speed

  roundSpeed :: Number -> Int
  roundSpeed n = case fromNumber $ round n of
    Just i -> i
    Nothing -> 2

createRandomObstacle :: Number -> Obstacle
createRandomObstacle speed =
  { emoji:
      case obstacleEmojis !! i of
        Just e -> e
        Nothing -> E.rock
  , x:
      case fromBottom of
        true -> rand 0 320
        false -> 320
  , y:
      case fromBottom of
        true -> 0
        false -> rand 0 240
  , size: rand 4 24
  , rotation: rand (-180) 180
  , speed
  }
  where
  i :: Int
  i = rand 0 9

  fromBottom :: Boolean
  fromBottom = case rand 0 1 of
    0 -> false
    _ -> true

  rand :: Int -> Int -> Int
  rand l h = unsafePerformEffect $ randomInt l h
