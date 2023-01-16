module Chunk.Gen (genChunks, genChunks') where

import Chunk
import Essentials

import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Prelude (error)

import qualified Integer.Positive as Positive
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-| Break up a text into a list of texts

    This can be useful for generating parser inputs for testing -}
genChunks :: Trivializable c => Nullable c -> Gen [c]
genChunks x = genChunksSeq x <&> LL.toList

genChunksSeq :: Trivializable c => Nullable c -> Gen (Seq c)
genChunksSeq x = case refine x of
    Nothing -> pure LL.empty
    Just y -> genChunksSeq' y

genChunks' :: Chunk c => c -> Gen [c]
genChunks' x = genChunksSeq' x <&> LL.toList

genChunksSeq' :: Chunk c => c -> Gen (Seq c)
genChunksSeq' x = Gen.recursive Gen.choice [pure (x :<| Empty)] [z x]

z :: Chunk c => c -> Gen (Seq c)
z x = case Positive.fromNatural (Positive.subtractOne (length x)) of
    Nothing -> pure (x :<| Empty)
    Just len -> do
      Just i <- Gen.integral (Range.constant 1 (Positive.toNatural len))
                  <&> Positive.fromNatural
      case split i x of
          SplitInsufficient -> error "genChunks: SplitInsufficient"
          Split a b -> pure (<>) <*> genChunksSeq' a <*> genChunksSeq' b
