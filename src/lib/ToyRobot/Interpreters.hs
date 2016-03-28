module ToyRobot.Interpreters (
  eval
) where

import           Linear.V2
import           Control.Monad.State
import           Control.Lens
import           Control.Monad.Free

import           ToyRobot.Types
import           ToyRobot.Models
import           ToyRobot.Actions


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


-- | Basic interpreter running in a beefy stack `m`
--------------------------------------------------------------------------------
eval :: forall (m :: * -> *). EvalStack World m => RobotProgram () -> m ()
eval (Free (Report next))    = (liftIO . print =<< get) >> eval next
eval (Free (Place p next))   = robot.location .= _place p >> eval next
eval (Free (Move next))      = ((robot.location %=) . _move =<< use (robot.direction)) >> eval next
eval (Free (TurnLeft next))  = robot.direction %= _turnLeft >> eval next
eval (Free (TurnRight next)) = robot.direction %= _turnRight >> eval next
eval (Free Done) = return ()
eval (Pure r) = return r

