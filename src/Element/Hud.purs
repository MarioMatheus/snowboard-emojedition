module Element.Hud where

import Prelude
import Data.Array (length, range, reverse, zip, (!!))
import Data.Char (toCharCode)
import Data.Int (decimal, toStringAs)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple, fst, snd)
import Emo8.Data.Emoji as E

type HudScoreElement
  = { emoji :: E.Emoji
    , x :: Int
    , y :: Int
    , size :: Int
    }

emojiList :: Array E.Emoji
emojiList =
  [ E.twelveOClock
  , E.oneOClock
  , E.twoOClock
  , E.threeOClock
  , E.fourOClock
  , E.fiveOClock
  , E.sixOClock
  , E.sevenOClock
  , E.eightOClock
  , E.nineOClock
  ]

getScoreHud :: Int -> Array HudScoreElement
getScoreHud score = map toScoreElement enumeratedEmojis
  where
  toScoreElement :: (Tuple Int E.Emoji) -> HudScoreElement
  toScoreElement t =
    { emoji: snd t
    , size: 10
    , y: 220
    , x: 300 - 12 * (fst t)
    }

  enumeratedEmojis :: Array (Tuple Int E.Emoji)
  enumeratedEmojis = zip (range 0 $ length hudEmojis) (reverse hudEmojis)

  hudEmojis :: Array E.Emoji
  hudEmojis = map toEmoji scoreChars

  scoreChars :: Array Char
  scoreChars = toCharArray $ (toStringAs decimal score)

  toEmoji :: Char -> E.Emoji
  toEmoji c = fromMaybe E.twelveOClock (emojiList !! (toCharCode c - 48))