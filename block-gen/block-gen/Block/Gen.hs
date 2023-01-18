{-| This can be useful for generating parser inputs for testing. -}
module Block.Gen (genBlocks, genBlocks') where

import Block.Class
import Essentials

import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Prelude (error)

import qualified Integer.Positive as Positive
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-| Break up a possibly-empty value into a list of blocks -}
genBlocks :: Trivializable block => Nullable block -> Gen [block]
genBlocks x = genBlocksSeq x <&> LL.toList

genBlocksSeq :: Trivializable block => Nullable block -> Gen (Seq block)
genBlocksSeq x = case refine x of
    Nothing -> pure LL.empty
    Just y -> genBlocksSeq' y

{-| Break up a block into a list of blocks -}
genBlocks' :: Block block => block -> Gen [block]
genBlocks' x = genBlocksSeq' x <&> LL.toList

genBlocksSeq' :: Block block => block -> Gen (Seq block)
genBlocksSeq' x = Gen.recursive Gen.choice [pure (x :<| Empty)] [z x]

z :: Block c => c -> Gen (Seq c)
z x = case Positive.fromNatural (Positive.subtractOne (length x)) of
    Nothing -> pure (x :<| Empty)
    Just len -> do
      Just i <- Gen.integral (Range.constant 1 (Positive.toNatural len))
                  <&> Positive.fromNatural
      case split i x of
          SplitInsufficient -> error "genBlocks: SplitInsufficient"
          Split a b -> pure (<>) <*> genBlocksSeq' a <*> genBlocksSeq' b
