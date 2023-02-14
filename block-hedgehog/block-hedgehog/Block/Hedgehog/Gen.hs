{-| This can be useful for generating parser inputs for testing. -}
module Block.Hedgehog.Gen (genBlocks, genBlocks') where

import Block.Class
import Essentials

import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Integer (Positive, Natural)
import Prelude (error)

import qualified Integer
import qualified Integer.Positive as Positive
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-| Break up a possibly-empty value into a list of blocks -}
genBlocks :: Refined block => Nullable block -> Gen [block]
genBlocks x = genBlocksSeq x <&> LL.toList

genBlocksSeq :: Refined block => Nullable block -> Gen (Seq block)
genBlocksSeq x = case refine x of
    Nothing -> pure LL.empty
    Just y -> genBlocksSeq' y

{-| Break up a block into a list of blocks -}
genBlocks' :: Block block => block -> Gen [block]
genBlocks' x = genBlocksSeq' x <&> LL.toList

genBlocksSeq' :: Block block => block -> Gen (Seq block)
genBlocksSeq' x =
    Gen.recursive Gen.choice [ stopSplitting ] [ keepSplitting ]
  where
    stopSplitting = pure (x :<| Empty)
    keepSplitting = split x >>= \case
        Nothing -> pure (x :<| Empty)
        Just (a, b) -> (<>) <$> genBlocksSeq' a <*> genBlocksSeq' b

split :: Block xs => xs -> Gen (Maybe (xs, xs))
split x = case Positive.fromNatural (Positive.subtractOne (length x)) of
    Nothing -> pure Nothing
    Just len -> do
      i <- positive len
      case take Front i x of
          TakePart a b -> pure (Just (a, b))
          _ -> error "Block.Hedgehog.Gen.split: 'take' out of bounds"

positive :: Positive -> Gen Positive
positive max = do
    i :: Natural <- Gen.integral (Range.constant 1 (Positive.toNatural max))
    pure (Integer.yolo i)
