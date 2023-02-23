A *block* is a non-empty sequence of items.


## Block types

The block types offered by this package:

  - `Text1`, a non-empty `Text` value whose item type is `Char`
  - `ByteString1`, a non-empty `ByteString` value whose item type is `Word8`
  - `Seq1`, a non-empty `Seq`.
  - `BlockBlock`, consisting of one block type nested within another
  - `NonEmpty`, though some operations are inefficient

To define your own block type, you will need a dependency on the `block-class`
package. You may also find it helpful to use the `Block.Null` module from the
`block-types` package.


## Front and back

All of the operations in this package's classes are symmetric; you can traverse
a block's contents either forwards or backwards. Most functions have an `End`
parameter specifying which end of the block we're working with or starting from.

```haskell
data End = Front | Back
```
