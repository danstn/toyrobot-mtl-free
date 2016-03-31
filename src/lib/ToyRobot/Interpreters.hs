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
sandbox :: forall (m :: * -> *). EvalStack World m => FreeRobot () -> m ()
sandbox (Pure r) = return r
sandbox (Free x) = case x of
  Report next     -> (liftIO . print =<< get) >> sandbox next
  Place p next    -> robot.location .= _place p >> sandbox next
  Move next       -> ((robot.location %=) . _move =<< use (robot.direction)) >> sandbox next
  TurnLeft next   -> robot.direction %= _turnLeft >> sandbox next
  TurnRight next  -> robot.direction %= _turnRight >> sandbox next
  Steer f         -> (liftIO $ putStr "robot#repl>") >> liftIO getLine >>= sandbox . f
  BadCommand next -> (liftIO $ putStrLn "bad command") >> sandbox next
  Done            -> return ()

-- | State interprete
{----------------------------------------------------------------------------------}
{-stateInterp :: MonadState World m => RobotProgram () -> m ()-}
{-stateInterp (Pure r) = return r-}
{-stateInterp (Free x) = case x of-}
  {-Report next    -> stateInterp next-}
  {-Place p next   -> robot.location .= _place p >> stateInterp next-}
  {-Move next      -> ((robot.location %=) . _move =<< use (robot.direction)) >> stateInterp next-}
  {-TurnLeft next  -> robot.direction %= _turnLeft >> stateInterp next-}
  {-TurnRight next -> robot.direction %= _turnRight >> stateInterp next-}
  {-Steer _        -> return ()-}
  {-Done           -> return ()-}

-- Interpreter

interactive :: InteractionF a -> IO a
interactive (Tell msg a) = putStrLn msg >> return a

robotic :: RobotF a -> IO a
robotic (Report a) = putStrLn "reporting" >> return a
robotic _ = undefined

{-roboticState :: MonadState World m => RobotF b -> m b-}
{-roboticState (Report a) = return a-}


{-sandbox :: forall (m :: * -> *). EvalStack World m =>-}

program :: Free Program ()
program = do
  tell "trying"
  report
  tell "hard..."

{-interpreters :: Coproduct InteractionF RobotF c -> IO c-}
interpreters = interactive |*| robotic

runProgram :: Free Program () -> IO ()
runProgram = magicFold (interpreters . run)
