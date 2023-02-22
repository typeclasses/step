module Block.Hedgehog.Gen.Positive (positive) where

import Block.Class
import Essentials

import Data.Sequence (Seq (..))
import Hedgehog (Gen)
import Integer (Positive, Natural)
import Prelude (error)

import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Integer
import qualified Integer.Positive as Positive

positive :: Positive -> Gen Positive
positive max = Gen.integral (Range.constant 1 (Positive.toNatural max))
    <&> (Integer.yolo :: Natural -> Positive)
