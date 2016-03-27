module Main where

import           Data.Maybe
import qualified Data.Map as Map
import           Linear.V2 (V2(..))
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Free
import           Control.Lens

type Name = String
type Position = V2 Integer
type Env = Map.Map String String

-- | Robot data structure
data Robot = Robot { _position :: V2 Integer
                   , _name :: String
                   } deriving (Show)

-- | Worlds data structure
data World = World { _robot :: Robot
                   } deriving (Show)

makeLenses ''World
makeLenses ''Robot

-- | Robot DSL
data RobotDSL next = Place (Integer, Integer) next
                     | Report next
                     | Done
                     deriving (Functor, Show)


-- | Robot Program definition and front-end
--------------------------------------------------------------------------------
type RobotProgram = Free RobotDSL

place :: (Integer, Integer) -> RobotProgram ()
place p = liftF $ Place p ()

report :: RobotProgram ()
report = liftF $ Report ()

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


-- | Basic interpreter running in a beefy stack `m`
--------------------------------------------------------------------------------
eval :: forall (m :: * -> *). EvalStack World m => RobotProgram () -> m ()
eval (Free (Report next)) = (liftIO . print =<< get) >> eval next
eval (Free (Place (x,y) next)) = (robot.position.= V2 x y) >> eval next
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
    _position = V2 0 0,
    _name = "Wall-e"
  }
}

execSandbox :: RobotProgram () -> IO (EvalResult ())
execSandbox = (runEval defaultEnv initialState) . eval
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Bazinga"
