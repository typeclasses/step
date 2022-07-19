module Step.Nontrivial.SplitAtPositive
  (
    splitAtPositive, SplitAtPositive (..),
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial

import qualified ListLike

import qualified Positive
import qualified Positive.Math as Positive

import Signed (Signed)
import qualified Signed as Signed

import qualified Maybe

data SplitAtPositive text char = All | Insufficient (Positive Natural) | Split (Nontrivial text char) (Nontrivial text char)

splitAtPositive :: ListLike text char => Positive Natural -> Nontrivial text char -> SplitAtPositive text char
splitAtPositive n whole =
    case Positive.minus (Nontrivial.length whole) n of
        Signed.Zero -> All
        Signed.Plus _ ->
            let (a, b) = ListLike.splitAt (fromIntegral (review Positive.refine n)) (Nontrivial.generalize whole)
            in Split (NontrivialUnsafe a) (NontrivialUnsafe b)
        Signed.Minus n' ->
            Insufficient n'
