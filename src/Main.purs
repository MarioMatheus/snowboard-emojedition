module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Emo8 (emo8)
import Emo8.Data.Color as C
import Emo8.Data.Emoji as E
import Emo8.Game (class Game)
import Emo8.Game.Update (Update, getCanvasSize)
import Emo8.Game.Draw (cls, emor')
import Emo8.Type (Size)
import Emo8.Util.Collide (sinkCanvas)
import Element.Player (Player, updatePlayer)

-- import Debug.Trace (trace)
data GameState
  = State
    { player :: Player
    }

instance gameState :: Game GameState where
  update input (State state) = do
    updatedPlayer <- system state.player
    pure
      $ State { player: updatedPlayer }
    where
    system :: Player -> Update Player
    system =
      canvasCollision
        <<< updatePlayer'

    updatePlayer' :: Player -> Player
    updatePlayer' = updatePlayer input

    canvasCollision :: Player -> Update Player
    canvasCollision player = do
      r <- getCanvasSize
      pure case sinkCanvas r emoSize player.x player.y of
        Just s -> player { x = player.x - s.x, y = player.y - s.y }
        Nothing -> player
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
