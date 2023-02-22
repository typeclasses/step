{-| This can be useful for generating parser inputs for testing.

A larger size parameter results in splitting up the block into
smaller and more numerous parts. -}
module Block.Hedgehog.Gen.Shatter (shatter1, shatter0) where

import Block.Class
import Essentials

import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Integer (Positive, Natural)
import Prelude (error)
import Block.Hedgehog.Gen.Positive (positive)

import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Integer
import qualified Integer.Positive as Positive

{-| Break up a block into a list of blocks -}
shatter1 :: Positional xs => xs -> Gen [xs]
shatter1 x = shatterSeq1 x <&> LL.toList

{-| Break up a possibly-empty value into a list of blocks -}
shatter0 :: (Positional xs, Refined nul xs) => nul -> Gen [xs]
shatter0 = refine >>> maybe (pure []) (shatterSeq1 >>> fmap LL.toList)

shatterSeq1 :: Positional xs => xs -> Gen (Seq xs)
shatterSeq1 x = Gen.recursive Gen.choice [ stopSplitting ] [ keepSplitting ]
  where
    stopSplitting = pure (x :<| Empty)
    keepSplitting = split x & maybe stopSplitting \g ->
        g >>= \(a, b) -> (<>) <$> shatterSeq1 a <*> shatterSeq1 b

split :: Positional xs => xs -> Maybe (Gen (xs, xs))
split xs = xs & length & Positive.subtractOne & Positive.fromNatural
    <&> \len -> positive len <&> \i -> xs & take Front i & requireTakePart
  where
    requireTakePart = \case { TakePart a b -> (a, b);
      _ -> error "Block.Hedgehog.Gen.split: 'take' out of bounds" }
