module Main where

import qualified Data.Map as Map
import           Linear.V2
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Free
import           Control.Lens

type Name = String
type Env = Map.Map String String

data Direction = North | East | South | West deriving (Show, Bounded, Enum)

data Robot = Robot { _location :: V2 Integer
                   , _direction :: Direction
                   , _name :: String
                   } deriving (Show)

data World = World { _robot :: Robot
                   } deriving (Show)

makeLenses ''World
makeLenses ''Robot

-- | Robot DSL
data RobotDSL next = Place (Integer, Integer) next
                     | Report next
                     | Move next
                     | TurnLeft next
                     | TurnRight next
                     | Done
                     deriving (Functor, Show)


-- | Robot API front-end
--------------------------------------------------------------------------------
type RobotProgram = Free RobotDSL

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
--------------------------------------------------------------------------------


-- | Execution monad stack & a runner
--------------------------------------------------------------------------------
type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT World IO))) a

type EvalStack s m = (MonadIO m
                    , MonadState s m
                    , MonadError String m
                    , MonadReader Env m
                    , MonadWriter [Name] m)

type EvalResult a = ((Either String a, [String]), World)

runEval :: Env -> World -> Eval a -> IO (EvalResult a)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st
--------------------------------------------------------------------------------

-- | Robot API back-end (actions implementation details)
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
--------------------------------------------------------------------------------


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
--------------------------------------------------------------------------------


-- | Sandbox
--------------------------------------------------------------------------------
defaultEnv :: Env
defaultEnv = Map.empty

initialState :: World
initialState = World {
  _robot = Robot {
    _location = V2 0 0,
    _direction = North,
    _name = "Wall-e"
  }
}

execSandbox :: RobotProgram () -> IO (EvalResult ())
execSandbox = (runEval defaultEnv initialState) . eval
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Bazinga"
