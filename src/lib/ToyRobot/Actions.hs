module ToyRobot.Actions where

import           Control.Monad (forever)
import           Control.Monad.Free

-- | Robot DSL
--------------------------------------------------------------------------------
data RobotDSL next = Place (Integer, Integer) next
                     | Report next
                     | Move next
                     | TurnLeft next
                     | TurnRight next
                     | Done
                     deriving (Functor, Show)

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

done :: RobotProgram ()
done = liftF Done


