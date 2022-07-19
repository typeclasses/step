module Step.Nontrivial.SplitAtPositive where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

import Step.Nontrivial.Span

splitAtPositive :: ListLike text char => Positive Natural -> Nontrivial text char -> (Nontrivial text char, Maybe (Nontrivial text char))
splitAtPositive n whole =
    let (a, b) = ListLike.splitAt (fromIntegral $ review Positive.natPrism n) (Nontrivial.generalize whole) in
    (NontrivialUnsafe a, Nontrivial.refine b)
