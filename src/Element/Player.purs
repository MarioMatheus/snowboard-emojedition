module Element.Player where

import Prelude
import Data.Ord (signum)
import Emo8.Data.Input (Input)

playerSpeed :: { x :: Int, y :: Int }
playerSpeed = { x: 3, y: 1 }

playerRotation :: Int
playerRotation = 2

type Player
  = { x :: Int
    , y :: Int
    , rotation :: Int
    }

type UpdateData
  = { player :: Player
    , input :: Input
    }

movementH :: UpdateData -> UpdateData
movementH data' = case data'.input.isLeft, data'.input.isRight of
  true, false -> data' { player { x = x - playerSpeed.x, y = y - playerSpeed.y } }
  false, true -> data' { player { x = x + playerSpeed.x, y = y + playerSpeed.y } }
  _, _ -> data'
  where
  { x, y } = data'.player

movementV :: UpdateData -> UpdateData
movementV data' = case data'.input.isUp, data'.input.isDown of
  true, false -> data' { player { x = x - playerSpeed.y, y = y + playerSpeed.y } }
  false, true -> data' { player { x = x + playerSpeed.y, y = y - playerSpeed.x } }
  _, _ -> data'
  where
  { x, y } = data'.player

rotate :: UpdateData -> UpdateData
rotate data' = case isLeft || isDown, isRight || isUp of
  true, false -> updateRotation $ max (-16) rotation - playerRotation
  false, true -> updateRotation $ min 16 rotation + playerRotation
  _, _ ->
    updateRotation case rotation of
      0 -> 0
      _ -> rotation - signum rotation
  where
  { rotation } = data'.player

  { isLeft, isRight, isUp, isDown } = data'.input

  updateRotation :: Int -> UpdateData
  updateRotation rotation' = data' { player { rotation = rotation' } }

updatePlayer :: Input -> Player -> Player
updatePlayer input player = _.player $ update { input, player }
  where
  update :: UpdateData -> UpdateData
  update =
    rotate
      <<< movementH
      <<< movementV
