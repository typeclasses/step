{-| This can be useful for generating parser inputs for testing. -}
module Block.Hedgehog.Gen.Shatter (shatterNullable, shatterBlock) where

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
shatterNullable :: Refined block => Nullable block -> Gen [block]
shatterNullable x = shatterNullableSeq x <&> LL.toList

shatterNullableSeq :: Refined block => Nullable block -> Gen (Seq block)
shatterNullableSeq x = case refine x of
    Nothing -> pure LL.empty
    Just y -> shatterBlockSeq y

{-| Break up a block into a list of blocks -}
shatterBlock :: Block block => block -> Gen [block]
shatterBlock x = shatterBlockSeq x <&> LL.toList

shatterBlockSeq :: Block block => block -> Gen (Seq block)
shatterBlockSeq x =
    Gen.recursive Gen.choice [ stopSplitting ] [ keepSplitting ]
  where
    stopSplitting = pure (x :<| Empty)
    keepSplitting = split x >>= \case
        Nothing -> pure (x :<| Empty)
        Just (a, b) -> (<>) <$> shatterBlockSeq a <*> shatterBlockSeq b

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
