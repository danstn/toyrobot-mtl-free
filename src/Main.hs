module Main where

import           Control.Monad

import           ToyRobot.Types
import           ToyRobot.Models
import           ToyRobot.Actions
import           ToyRobot.Interpreters

execSandbox :: RobotProgram () -> IO (EvalResult World ())
execSandbox = (runEval defaultEnv defaultWorld) . eval

main :: IO ()
main = do
  putStrLn "Bazinga"
