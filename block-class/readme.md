A *block* is a non-empty sequence of items.

Blocks and items are typically represented by type variables named `xs` and `x`.
The classes defined in this package link each block type to its corresponding
item type using multi-parameter type classes, with functional dependencies to
impose the constraint that each block must have one particular item type.


## Examples of block types

The `block-class` package, aiming to be minimal in its dependencies, provides
instances for only one type: `NonEmpty`. The `NonEmpty` instances are intended
primarily for demonstration purposes. Some of the `NonEmpty` operations are
asymptotically poor, particularly those that deal with the list from its back
end.

Some more useful examples of blocks may be found in the [block-types] package:

  - `Text1` is a non-empty `Text` value whose item type is `Char`.
  - `ByteString1` is a non-empty `ByteString` value whose item type is `Word8`.
  - `Seq1` is a non-empty `Seq`.
  - `BlockBlock` consists of one block type nested within another; for example,
    `BlockBlock Char Text1 (Seq1 Text1)` is the non-empty kin of `Seq Text`.


## Front and back

All of the operations in this package's classes are symmetric; you can traverse
a block's contents either forwards or backwards. Most functions have an `End`
parameter specifying which end of the block we're working with or starting from.

```haskell
data End = Front | Back
```


## Concatenation

The following have the constraint `Concat xs`.

```haskell
(++)   :: xs -> xs -> xs
concat :: End -> NonEmpty xs -> xs
```

The `Concat` class is roughly analogous to `Semigroup`.

The `(++)` operator concatenates two blocks.

If `(++)` is analogous to `(<>)`, then `concat` is roughly analogous to
`sconcat`. The difference is that `concat` has an `End` parameter.
`concat Front` concatenates in the normal way; `concat Back` concatenates in
reverse order.


## Item equality

We admit the possibility that there may be more than one way for a block to
represent any particular list of items. For example, a `BlockBlock` consisting
of lists of lists of characters may represent the three-character sequence
`"abc"` as `["abc"]`, `["ab", "c"]` or `["a", "bc"]`. Although these values
represent the same items, the `BlockBlock` values are not equal according to the
`(==)` operator.

So we have new class, `ItemEquality`, that can be used instead of the `Eq` class
when all you care about is comparing the items of a block.

```haskell
sameItems :: ItemEquality xs => xs -> xs -> Bool
```

## NonEmpty

The following have the constraint `NonEmptyIso x xs`.

```haskell
toNonEmpty   :: End -> xs -> NonEmpty x
fromNonEmpty :: End -> NonEmpty x -> xs
```

The `NonEmptyIso` class describes how a block is isomorphic (up to
`ItemEquality`) to `NonEmpty x`. The following properties should hold:

 1. If we convert a `NonEmpty` to a block and back, we should get
    back the same list.

    ```haskell
    toNonEmpty (fromNonEmpty ne) == ne
    ```

 2. If we convert a block to `NonEmpty` and back, the resulting
    block should have the same items as the original block.

    ```haskell
    fromNonEmpty (toNonEmpty xs) `sameItems` xs
    ```


## First and last items

The following all have the constraint `Singleton x xs`.

```haskell
singleton :: x -> xs
pop       :: End -> xs -> Pop x xs
unpop     :: End -> Pop x xs -> xs
push      :: End -> x -> xs -> xs
terminal  :: End -> xs -> x
first     :: xs -> x
last      :: xs -> x
pushMaybe :: End -> Maybe x -> Maybe xs -> Maybe xs
```


## Splitting by length

The following have the constraint `Positional xs`.

```haskell
length :: xs -> Positive
take   :: End -> Positive -> xs -> Take xs
```

```haskell
data Take xs =
    TakePart{ taken :: xs, takeRemainder :: xs }
  | TakeAll
  | TakeInsufficient Shortfall
```

```haskell
newtype Shortfall = Shortfall Positive
```


## Items by position

```haskell
at :: Index x xs => End -> Positive -> xs -> Maybe x
```


## Search

The following have the constraint `Search xs`.

```haskell
spanPredicate :: End -> (x -> Bool) -> xs -> Span xs
findPredicate :: End -> (x -> Bool) -> xs -> Maybe (Pivot x xs)
span          :: End -> (x -> State s Bool) -> xs -> State s (Span xs)
find          :: End -> (x -> State s (Maybe found))
                     -> xs -> State s (Maybe (Pivot found xs))
```

```haskell
data Span xs =
    SpanPart{ spanned :: xs, spanRemainder :: xs }
  | SpanNone
  | SpanAll
```

```haskell
data Pivot found xs = Pivot (Maybe xs) found (Maybe xs)
```


## Refinements of possibly-empty lists

For some types of block, there may exist a corresponding type which admits the
possibility of representing an empty sequence. Such types are called `Refined`.
For example, `Text1` is a refined type, and its unrefined precursor is `Text`.

The following have the constraint `Refined nul xs`.

```haskell
generalize :: xs -> nul
refine     :: nul -> Maybe xs
assume     :: nul -> xs
```

`assume` is defined only where `refine` produces `Just`, and it has a default
definition of `refine >>> fromJust`.


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


## Testing

A fairly extensive suite of property tests may be found in the [block-hedgehog]
package; you can use them to test your own instances.

The `block-class` package has a more modest test suite for itself; running the
tests for `block-hedgehog` provides some supplementary assurance that
`block-class` is correct.


  [block-types]: https://hackage.haskell.org/package/block-types

  [block-hedgehog]: https://hackage.haskell.org/package/block-hedgehog
