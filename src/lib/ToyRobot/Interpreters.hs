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
eval :: forall (m :: * -> *). EvalStack World m => RobotProgram () -> m ()
eval (Pure r) = return r
eval (Free x) = case x of
  Report next    -> (liftIO . print =<< get) >> eval next
  Place p next   -> robot.location .= _place p >> eval next
  Move next      -> ((robot.location %=) . _move =<< use (robot.direction)) >> eval next
  TurnLeft next  -> robot.direction %= _turnLeft >> eval next
  TurnRight next -> robot.direction %= _turnRight >> eval next
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
  Done           -> return ()


data Interaction x = Prompt (String -> x)
                   | Exec String (RobotProgram () -> x)
    deriving Functor

type RobotInteraction a = Free Interaction a

prompt :: RobotInteraction String
prompt = liftF $ Prompt id

-- Collapse our IOFree DSL into IO monad actions.
loop :: RobotInteraction a -> IO a
loop (Pure r) = return r
loop (Free x) = case x of
  Prompt f -> putStr "robot>" >> runSandbox move >> getLine >> (loop . f) "prompt"

repl :: IO ()
repl = forever $ loop prompt

runSandbox = (runEval defaultEnv defaultWorld) . eval

runRobot :: RobotProgram () -> IO (EvalResult World ())
runRobot = (runEval defaultEnv defaultWorld) . eval
