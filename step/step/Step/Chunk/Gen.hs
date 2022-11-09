module Step.Chunk.Gen (genChunks, genChunks') where

import Step.Chunk

import Control.Applicative (pure, (<*>))
import Data.Functor ((<&>))
import Data.Maybe (Maybe (..))
import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import Numeric.Natural (Natural)
import Optics (preview, review)
import Prelude (error)
import Data.Semigroup (Semigroup (..))

import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

{-| Break up a text into a list of texts

    This can be useful for generating parser inputs for testing
-}
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
z x = case Positive.minus (length x) one of
    Signed.Zero -> pure (x :<| Empty)
    Signed.Plus len -> do
      Just i <- Gen.integral (Range.constant 1 (review Positive.refine len))
                  <&> preview Positive.refine
      case split i x of
          SplitInsufficient -> error "genChunks: SplitInsufficient"
          Split a b -> pure (<>) <*> genChunksSeq' a <*> genChunksSeq' b
    Signed.Minus _ -> error "Step.Chunk.ListLike: minus one cannot be negative"

one :: Positive Natural
one = PositiveUnsafe 1
