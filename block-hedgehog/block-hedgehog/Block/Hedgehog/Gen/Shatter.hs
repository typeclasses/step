{-| This can be useful for generating parser inputs for testing.

A larger size parameter results in splitting up the block into
smaller and more numerous parts. -}
module Block.Hedgehog.Gen.Shatter (shatter0, shatter1) where

import Block.Class
import Essentials

import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Prelude (error)

import qualified Block.Hedgehog.Gen.Positive as Gen
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Hedgehog.Gen as Gen
import qualified Integer.Positive as Positive

{-| Break up a possibly-empty value into a list of blocks -}
shatter0 :: (Positional xs, Refined nul xs) => nul -> Gen [xs]
shatter0 = refine >>> maybe (pure []) (shatterSeq1 >>> fmap Foldable.toList)

{-| Break up a block into a list of blocks -}
shatter1 :: Positional xs => xs -> Gen (NonEmpty xs)
shatter1 x = shatterSeq1 x
    <&> (Foldable.toList >>> NonEmpty.nonEmpty >>> Maybe.fromJust)

shatterSeq1 :: Positional xs => xs -> Gen (Seq xs)
shatterSeq1 x = Gen.recursive Gen.choice [ stopSplitting ] [ keepSplitting ]
  where
    stopSplitting = pure (x :<| Empty)
    keepSplitting = split x & maybe stopSplitting \g ->
        g >>= \(a, b) -> (<>) <$> shatterSeq1 a <*> shatterSeq1 b

split :: Positional xs => xs -> Maybe (Gen (xs, xs))
split xs = xs & length & Positive.subtractOne & Positive.fromNatural
    <&> \len -> Gen.positive len <&> \i -> xs & take Front i & requireTakePart
  where
    requireTakePart = \case { TakePart a b -> (a, b);
      _ -> error "Block.Hedgehog.Gen.split: 'take' out of bounds" }
