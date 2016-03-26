# ToyRobot MTL 🆓

Implementation of a ToyRobot using Monad Transformers (mtl) to represent state, logging, environment & error handling.

Robot's DSL is wrapped in a Free Monad.

### Robot DSL

|Instruction|Arguments|API|Description|
|---|---|---|---|
|Place|(Integer, Integer)|`place (x,y)`|Place a robot at (x,y)|
|Report|-|`report`|Report current state (position atm)|
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
λ> execSandbox $ place (1,1) >> report >> place (2,6) >> report
(1,1)
(2,6)
((Right (),[]),(2,6))
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
```

#### Monad Stack Example (with Super Powers!)

```haskell
type EvalStack s m =
  (Show s, MonadIO m, MonadState s m, MonadError String m, MonadReader Env m, MonadWriter [Name] m)
```
