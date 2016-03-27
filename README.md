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


### Usage

In `stack repl`:

***Inline evaluation***

Use `execSandbox` to run a sequence in a monad stack with super powers!

```haskell
λ> execSandbox $ <sequence>
```

*Example:*
```haskell
λ> execSandbox $ place (0,0) >> replicateM_ 2 move >> turnRight >> move >> report
World {_robot = Robot {_location = V2 1 2, _direction = East, _name = "Wall-e"}}
((Right (),[]),World {_robot = Robot {_location = V2 1 2, _direction = East, _name = "Wall-e"}})
```

***Building AST(s)***

By evaluating a sequence directly - you get an AST back.

```haskell
λ> place (1,3) >> report >> place (9,5) >> report >> done
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
