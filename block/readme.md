A *block* is a non-empty sequence of items.

The block types offered by this package:

  * `Text1`, a non-empty `Text` value whose item type is `Char`
  * `ByteString1`, a non-empty `ByteString` value whose item type is `Word8`
  * `Seq1`, a non-empty `Seq`.
  * `BlockBlock`, consisting of one block type nested within another
  * `NonEmpty`, though some operations are inefficient

Although the features of this package are divided among a handful of classes,
the central one to remember is the titular `Block` class, which is a subclass
of nearly all of them. Generally you can't go wrong by putting a `Block x xs`
constraint on your definitions, even if a more specific class might suffice.

To define your own block type, you will need a dependency on the `block-class`
package. You may also find it helpful to use the `Block.Null` module from the
`block-types` package.

Since blocks are non-empty, most of the functions involving lengths and
positions use the `Positive` type from the `integer-types` package.

All of the operations in this package's classes are symmetric; you can traverse
a block's contents either forwards or backwards. Most functions have an `End`
parameter (`Front` or `Back`) specifying which end of the block we're working
with or starting from.

Overview of basic operations supported by the `Block` class:

  * Use `length` to obtain the number of items in a block. The length type is
    `Positive`.

  * Concatenate two blocks with `(++)` and a list of blocks with `concat`.

  * Use `sameItems` rather than `(==)` to test whether two blocks contain the
    same items. Two `BlockBlock` values, for example, may have the same content
    with internal structural differences that make the blocks unequal according
    to `Eq` but equal according to `sameItems`.

  * A block can be converted back and forth with `NonEmpty` using `toNonEmpty`
    and `fromNonEmpty`.

  * Use `singleton` to construct a block with a single item.

  * Use `terminal` to obtain the block's first or last item. Since blocks are
    non-empty, this function is total, unlike the `head` function of regular
    lists.

  * The `push` function adds a single item to the front or back or a block.
    In other libraries this operation is sometimes called *cons* or *snoc*.

  * The `pop` function divide a block into its terminal item and the remainder.
    The result is `Pop = Pop x (Maybe xs)`. In other libraries this operation
    is sometimes called *uncons* or *unsnoc*.

  * Use `at` to obtain the item at a particular index. The index type is
    `Positive`. The first item has index 1.

Demonstrations of some more exciting operations can be found in the source code
of test suite.
