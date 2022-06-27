module Step.Test.InputChunking (genChunks) where

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListLike

-- | Generates various ways of chunking a given input text. Shrinks toward the smallest number of chunks.
genChunks :: ListLike chunk char => chunk -> Gen [chunk]
genChunks x =
    if ListLike.null x
    then Gen.integral (Range.linear 0 4) <&> \n -> ListLike.replicate n ListLike.empty
    else do
        numberOfChunks <- Gen.integral (Range.linear 1 (4 + ListLike.length x))
        ListLike.toList <$> execStateT (replicateM (numberOfChunks - 1) (modifyM fragment)) (ListLike.singleton x)

-- | From a list of chunks, pick one and divide it in some way
fragment :: MonadGen m => ListLike chunk char => Seq chunk -> m (Seq chunk)
fragment xs = do
    (before, target, after) <- genSplitAround xs
    (a, b) <- genSplit target
    return (ListLike.fold [before, ListLike.fromList [a, b], after])

-- | Split a chunk into two. The input chunk may be empty. May produce an empty chunk.
genSplit :: ListLike chunk char => MonadGen m => chunk -> m (chunk, chunk)
genSplit x = Gen.integral (Range.constant 0 (ListLike.length x)) <&> \i -> ListLike.splitAt i x

-- | Pick a random position in the sequence. Return a 3-tuple with the items (before, at, after) the chosen position. Undefined if the list is empty.
genSplitAround :: MonadGen m => Seq a -> m (Seq a, a, Seq a)
genSplitAround xs = Gen.integral (Range.constant 0 (ListLike.length xs - 1)) <&> \i -> splitAround i xs

-- | Return a 3-tuple with the items (before, at, after) the chosen position. Undefined if the index is out of range.
splitAround :: Int -> Seq a -> (Seq a, a, Seq a)
splitAround n xs = (a, b, c) where (a, ListLike.uncons -> Just (b, c)) = ListLike.splitAt n xs
