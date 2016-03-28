module ToyRobot.Controller where

import           Linear.V2

import           ToyRobot.Models

-- | Robot API back-end (interpreted action implementation details)
--------------------------------------------------------------------------------
_place :: Num a => (a, a) -> V2 a
_place (x, y) = V2 x y

_turnRight :: Direction -> Direction
_turnRight West = minBound
_turnRight d = succ d

_turnLeft :: Direction -> Direction
_turnLeft North = maxBound
_turnLeft d = pred d

_move :: Num a => Direction -> V2 a -> V2 a
_move North l = l + V2 0 1
_move East l  = l + V2 1 0
_move South l = l + V2 0 (-1)
_move West l  = l + V2 (-1) 0

