module Main where

import Prelude
-- import Debug.Trace (trace)
import Data.Ord (signum)
import Effect (Effect)
import Emo8 (emo8)
import Emo8.Data.Color as C
import Emo8.Data.Emoji as E
import Emo8.Game (class Game)
import Emo8.Game.Draw (cls, emor')
import Emo8.Type (Size)

data GameState
  = State
    { player :: Player
    }

type Player
  = { x :: Int
    , y :: Int
    , rotation :: Int
    }

instance gameState :: Game GameState where
  update input (State state) =
    pure
      $ State { player: updatedPlayer }
    where
    updatedPlayer :: Player
    updatedPlayer = system state.player

    system :: Player -> Player
    system =
      rotate
        <<< movement

    movement :: Player -> Player
    movement player = case input.isLeft, input.isRight of
      true, false -> player { x = player.x - 4, y = player.y - 1 }
      false, true -> player { x = player.x + 4, y = player.y + 1 }
      _, _ -> player

    rotate :: Player -> Player
    rotate player = case input.isLeft, input.isRight of
      true, false -> player { rotation = max (-16) player.rotation - 2 }
      false, true -> player { rotation = min 16 player.rotation + 2 }
      _, _ ->
        player
          { rotation =
            case player.rotation of
              0 -> 0
              _ -> player.rotation - signum player.rotation
          }
  draw (State state) = do
    cls C.snow
    emor' state.player.rotation E.snowboarder emoSize state.player.x state.player.y
  sound _ = pure unit

emoSize :: Size
emoSize = 32

initialState :: GameState
initialState =
  State
    { player:
        { x: 100
        , y: 150
        , rotation: 0
        }
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
