module Main where

import           Data.Maybe
import qualified Data.Map as Map
import           Linear.V2
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Free

type Name = String
type Position = V2 Integer
type Env = Map.Map String String

-- | Robot data structure
data Robot = Robot { position :: Position
                   } deriving (Show)

-- | Worlds data structure
data World = World { robot :: Robot
                   } deriving (Show)

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
eval (Free (Place (x,y) next)) = put (World {robot = Robot {position = V2 x y}}) >> eval next
eval (Free Done) = return ()
eval (Pure r) = return r
--------------------------------------------------------------------------------


-- | Sandbox
--------------------------------------------------------------------------------
defaultEnv :: Env
defaultEnv = Map.empty

defaultState :: World
defaultState = World { robot = Robot { position = V2 0 0 } }


execSandbox :: RobotProgram () -> IO (EvalResult ())
execSandbox = (runEval defaultEnv defaultState) . eval
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Bazinga"
