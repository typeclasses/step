
## State

The `Search` class operations use a `State` context so that you can gather
additional information as you search. For example:

```haskell
span :: Search x xs => End -> (x -> State s Bool) -> xs -> State s (Span xs)
```

This could have been defined with a more general monadic context:

```haskell
span :: Search x xs => Monad m => End -> (x -> m Bool) -> xs -> m (Span xs)
```

Unfortunately, `m` has a nominal type role in the latter signature, which would
preclude `newtype` or `via` deriving for the `Search` class. So we have instead
chosen to sacrifice some generality to permit deriving.

The `State` type from the `transformers` library poses the same problem, because
it is an alias for `StateT Identity`, and `StateT` also has a type parameter `m`
with a nominal role. Therefore we provide our own `State` type, which is not a
monad transformer, instead of using theirs.
