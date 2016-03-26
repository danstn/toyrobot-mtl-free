# ToyRobot MTL

Using mtl to build a monad stack for Robot DSL AST evaluation.

### Running

In `stack repl`:


```haskell
λ> execSandbox $ <sequence>
```

Example sequences:
```haskell
Done
Report Done
(Teleport (5,6) (Report (Teleport (9,4) (Report Done))))
(Teleport (1,1) (Report (Report Done)))
```

### Monad Stack Example

```haskell
type EvalStack s m =
  (Show s, MonadIO m, MonadState s m, MonadError String m, MonadReader Env m, MonadWriter [Name] m)
```
