module Main where

import           ToyRobot.Types
import           ToyRobot.Models
import           ToyRobot.Actions
import           ToyRobot.Interpreters
import           ToyRobot.CLI (repl)

play :: RobotProgram () -> IO (EvalResult World ())
play = (runEval defaultEnv defaultWorld) . sandbox

main :: IO ()
main = do
  putStrLn "--- Starting Robot REPL..."
  _ <- play repl
  return ()
