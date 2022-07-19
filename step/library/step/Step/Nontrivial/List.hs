{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.List
  (
    cons, positiveSplitAt, length, takeWhile,
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

import Step.Nontrivial.Span

cons :: ListLike text char => ListLike list (Nontrivial text char) => text -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine

positiveSplitAt :: ListLike text char => Positive Natural -> Nontrivial text char -> (Nontrivial text char, Maybe (Nontrivial text char))
positiveSplitAt n whole =
    let (a, b) = ListLike.splitAt (fromIntegral $ review Positive.natPrism n) (Nontrivial.generalize whole) in
    (NontrivialUnsafe a, Nontrivial.refine b)

length :: ListLike text char => Nontrivial text char -> Positive Natural
length = Maybe.fromJust . preview Positive.natPrism . fromIntegral . ListLike.length . Nontrivial.generalize

takeWhile :: ListLike text char => (char -> Bool) -> Nontrivial text char -> Maybe (Nontrivial text char)
takeWhile f = Nontrivial.refine . ListLike.takeWhile f . Nontrivial.generalize
