module ToyRobot.Interpreters where

import           Control.Monad.State
import           Control.Lens
import           Control.Monad.Free

import           ToyRobot.Types
import           ToyRobot.Models
import           ToyRobot.Actions
import           ToyRobot.Controller

-- | Basic interpreter running in a beefy stack `m`
--------------------------------------------------------------------------------
sandbox :: forall (m :: * -> *). EvalStack World m => RobotProgram () -> m ()
sandbox (Pure r) = return r
sandbox (Free x) = case x of
  Report next    -> (liftIO . print =<< get) >> sandbox next
  Place p next   -> robot.location .= _place p >> sandbox next
  Move next      -> ((robot.location %=) . _move =<< use (robot.direction)) >> sandbox next
  TurnLeft next  -> robot.direction %= _turnLeft >> sandbox next
  TurnRight next -> robot.direction %= _turnRight >> sandbox next
  Steer f        -> (liftIO $ putStr "robot#repl>") >> liftIO getLine >>= sandbox . f
  Done           -> return ()

-- | State interprete
--------------------------------------------------------------------------------
stateInterp :: MonadState World m => RobotProgram () -> m ()
stateInterp (Pure r) = return r
stateInterp (Free x) = case x of
  Report next    -> stateInterp next
  Place p next   -> robot.location .= _place p >> stateInterp next
  Move next      -> ((robot.location %=) . _move =<< use (robot.direction)) >> stateInterp next
  TurnLeft next  -> robot.direction %= _turnLeft >> stateInterp next
  TurnRight next -> robot.direction %= _turnRight >> stateInterp next
  Steer _        -> return ()
  Done           -> return ()

