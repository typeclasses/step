A *block* is a non-empty sequence of items.

Blocks and items are typically represented by type variables named `xs` and `x`.


## Examples of block types

The `block-class` package, aiming to be minimal in its dependencies, provides
instances for only one type: `NonEmpty`. The `NonEmpty` instances are intended
primarily for demonstration purposes. Some of the `NonEmpty` operations are
asymptotically poor, particularly those that deal with the list from its back
end.

Some more useful examples of blocks may be found in the [block-types] package;
for example, the `Text1` type defined there is a non-empty `Text` value whose
item type is `Char`. The `block-types` package also defines a type called
`BlockBlock`, which consists of one block type nested within another; for
example, a non-empty `Seq` of non-empty `Text`.


## Refined

For some types of block, there may exist a corresponding type which admits the
possibility of representing an empty sequence. Such types are called `Refined`.
For example, the unrefined precursor to `Text1` is `Text`.


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


  [block-types]: https://hackage.haskell.org/package/block-types
