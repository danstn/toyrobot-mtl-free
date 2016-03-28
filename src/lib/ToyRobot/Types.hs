module ToyRobot.Types where

import qualified Data.Map as Map
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State

type Name = String

type Env = Map.Map String String

type Eval s a = ReaderT Env (ExceptT String (WriterT [String] (StateT s IO))) a

type EvalStack s m = (MonadIO m
                    , MonadState s m
                    , MonadError String m
                    , MonadReader Env m
                    , MonadWriter [Name] m)

type EvalResult s a = ((Either String a, [String]), s)


-- | Execution monad stack & a runner
--------------------------------------------------------------------------------
runEval :: Env -> s -> Eval s a -> IO (EvalResult s a)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

defaultEnv :: Env
defaultEnv = Map.empty
