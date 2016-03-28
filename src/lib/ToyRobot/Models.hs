module ToyRobot.Models where

import           Linear.V2
import           Control.Lens

data Direction = North | East | South | West deriving (Show, Bounded, Enum)

data Robot = Robot { _location :: V2 Integer
                   , _direction :: Direction
                   , _name :: String
                   } deriving (Show)

data World = World { _robot :: Robot
                   } deriving (Show)

makeLenses ''World
makeLenses ''Robot

defaultWorld :: World
defaultWorld = World {
  _robot = Robot {
    _location = V2 0 0,
    _direction = North,
    _name = "Wall-e"
  }
}

