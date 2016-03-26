module Main where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Free

import           Data.Maybe
import qualified Data.Map as Map

type Name = String
type Position = (Integer, Integer)
type Env = Map.Map String String

data RobotDSL a next = Place a next
                     | Report next
                     | Done
                     deriving (Functor, Show)

place :: a -> Free (RobotDSL a) ()
place p = liftF $ Place p ()

report :: Free (RobotDSL a) ()
report = liftF $ Report ()

done :: Free (RobotDSL a) ()
done = liftF Done

type Eval a = ReaderT Env (ExceptT String (WriterT [String] (StateT Position IO))) a

type EvalStack s m = (MonadIO m
                    , MonadState s m
                    , MonadError String m
                    , MonadReader Env m
                    , MonadWriter [Name] m)

type EvalResult a = ((Either String a, [String]), Position)

runEval :: Env -> Position -> Eval a -> IO (EvalResult a)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval :: forall (m :: * -> *). EvalStack Position m => Free (RobotDSL Position) () -> m ()
eval (Free (Report next)) = (liftIO . print =<< get) >> eval next
eval (Free (Place pos next)) = put pos >> eval next
eval (Free Done) = return ()
eval (Pure r) = return r

execSandbox :: Free (RobotDSL Position) () -> IO (EvalResult ())
execSandbox = (runEval Map.empty (0, 0)) . eval

main :: IO ()
main = do
  putStrLn "Bazinga"
