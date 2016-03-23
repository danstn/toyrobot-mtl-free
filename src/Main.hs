module Main where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State

import           Data.Maybe
import qualified Data.Map as Map

type Name = String
type Position = (Integer, Integer)
type Env = Map.Map String String

data RobotAction = Teleport Position RobotAction
                 | Report RobotAction
                 | Done
                 deriving (Show)

type Eval a = ReaderT Env (ExceptT String
                           (WriterT [String]
                           (StateT Position IO))) a
type EvalStack s m =
  (Show s, MonadIO m, MonadState s m, MonadError String m, MonadReader Env m, MonadWriter [Name] m)

type EvalResult a = ((Either String a, [String]), Position)

runEval :: Env -> Position -> Eval a -> IO (EvalResult a)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval :: forall (m :: * -> *). EvalStack Position m => RobotAction -> m Position
eval (Teleport pos next) = put pos >> eval next
eval (Report next) = (liftIO . print =<< get) >> eval next
eval (Done) = get

execSandbox :: RobotAction -> IO (EvalResult Position)
execSandbox = (runEval Map.empty (0, 0)) . eval

main :: IO ()
main = do
  putStrLn "Bazinga"
