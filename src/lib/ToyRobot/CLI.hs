module ToyRobot.CLI (
  repl
) where

import           Data.Text
import           Text.Read (readMaybe)

import           Control.Monad (forever)

import           ToyRobot.Actions

data CLI = PLACE Integer Integer
                    | REPORT
                    | MOVE
                    | LEFT
                    | RIGHT
                    | EXIT
                    deriving (Read, Show)

{-cliExec :: CLI -> Program ()-}
{-cliExec (PLACE x y) = place (x, y)-}
{-cliExec REPORT      = report-}
{-cliExec MOVE        = move-}
{-cliExec LEFT        = turnLeft-}
{-cliExec RIGHT       = turnRight-}
{-cliExec EXIT        = done-}

repl = undefined
{-repl :: Program ()-}
{-repl = forever $ do-}
  {-line <- steer-}
  {-let cmd = (toUpper . strip . pack) line-}
  {-maybe badCommand cliExec (readMaybe $ unpack cmd :: Maybe CLI)-}
  {-repl-}

