module ToyRobot.Actions where

import           Control.Monad.Free

-- | Robot DSL
--------------------------------------------------------------------------------
data RobotDSL next = Place (Integer, Integer) next
                     | Report next
                     | Move next
                     | TurnLeft next
                     | TurnRight next
                     | Steer (String -> next)
                     | Done
                     deriving Functor

type RobotProgram = Free RobotDSL

-- | Robot API front-end
--------------------------------------------------------------------------------
place :: (Integer, Integer) -> RobotProgram ()
place p = liftF $ Place p ()

move :: RobotProgram ()
move = liftF $ Move ()

report :: RobotProgram ()
report = liftF $ Report ()

turnLeft :: RobotProgram ()
turnLeft = liftF $ TurnLeft ()

turnRight :: RobotProgram ()
turnRight = liftF $ TurnRight ()

steer :: RobotProgram String
steer = liftF $ Steer id

done :: RobotProgram ()
done = liftF Done


