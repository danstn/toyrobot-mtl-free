# Free ToyRobot (with mtl)

Implementation of a ToyRobot using Free.

### Robot DSL

|Instruction|Arguments|API|Description|
|---|---|---|---|
|Place|(Integer, Integer)|`place (x,y)`|Place a robot at (x,y)|
|Report|-|`report`|Report current state (position atm)|
|Move|-|`move`|Move the robot in the current direction|
|TurnLeft|-|`turnLeft`|Turn left|
|TurnRight|-|`turnRight`|Turn right|
|Done|-|`done`|Terminate evaluation|

***Note:*** CLI is coupled with RobotDSL atm (`Steer` action; not listed above). Will try using Coproducts to decouple.

### Example usage

In `stack repl`:


**Inline evaluation**

Use `play` to run a sequence in a monad stack with super powers!

```haskell
λ> play $ <sequence>
```

*Example:*
```haskell
λ> import Control.Monad -- to use replicateM_
λ> play $ place (0,0) >> replicateM_ 2 move >> turnRight >> move >> report
World {_robot = Robot {_location = V2 1 2, _direction = East, _name = "Wall-e"}}
((Right (),[]),World {_robot = Robot {_location = V2 1 2, _direction = East, _name = "Wall-e"}})
```

**Repl**

```haskell
λ> play repl
robot#repl>report
World {_robot = Robot {_location = V2 0 0, _direction = North, _name = "Wall-e"}}
robot#repl>
```

**Building AST(s)**

***Note:*** `printingInterpreter` does not exist - write it yourself!

```haskell
λ> printingInterpreter $ place (1,3) >> report >> place (9,5) >> report >> done
Free (Place (1,3) (Free (Report (Free (Place (9,5) (Free (Report (Free Done))))))))
```

You can now use the generated AST however you want in your interpreter!

**Example sequences:**
```haskell
done
report >> done
place (1,1) >> report >> place (2,6) >> done >> report
report >> place (20, 5) >> report >> done
turnRight >> turnLeft >> replicateM_ 2 turnRight
replicateM_ 5 turnLeft
place (0,0) >> replicateM_ 2 move >> turnRight >> move >> report
```

#### Monad Stack Example (with Super Powers!)

```haskell
type EvalStack s m =
  (Show s, MonadIO m, MonadState s m, MonadError String m, MonadReader Env m, MonadWriter [Name] m)
```
