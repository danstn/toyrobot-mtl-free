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
import           ToyRobot.Controller

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

