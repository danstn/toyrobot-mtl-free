module Main where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Free

import           Data.Maybe
import qualified Data.Map as Map

import           Linear.V2

type Name = String
type Position = V2 Integer
type Env = Map.Map String String

data Robot = Robot { position :: Position
                   } deriving (Show)

data World = World { robot :: Robot
                   } deriving (Show)

data RobotDSL a next = Place (Integer, Integer) next
                     | Report next
                     | Done
                     deriving (Functor, Show)

place :: (Integer, Integer) -> Free (RobotDSL a) ()
place p = liftF $ Place p ()

report :: Free (RobotDSL a) ()
report = liftF $ Report ()

done :: Free (RobotDSL a) ()
done = liftF Done

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT World IO))) a

type EvalStack s m = (MonadIO m
                    , MonadState s m
                    , MonadError String m
                    , MonadReader Env m
                    , MonadWriter [Name] m)

type EvalResult a = ((Either String a, [String]), World)

runEval :: Env -> World -> Eval a -> IO (EvalResult a)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval :: forall (m :: * -> *). EvalStack World m => Free (RobotDSL World) () -> m ()
eval (Free (Report next)) = (liftIO . print =<< get) >> eval next
eval (Free (Place (x,y) next)) = put (World {robot = Robot {position = V2 x y}}) >> eval next
eval (Free Done) = return ()
eval (Pure r) = return r

defaultEnv :: Env
defaultEnv = Map.empty

defaultState :: World
defaultState = World { robot = Robot { position = V2 0 0 } }

execSandbox :: Free (RobotDSL World) () -> IO (EvalResult ())
execSandbox = (runEval defaultEnv defaultState) . eval

main :: IO ()
main = do
  putStrLn "Bazinga"
