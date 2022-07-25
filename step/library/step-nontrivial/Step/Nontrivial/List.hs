{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.List
  (
    cons,
    length, lengthNat, lengthInt,
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Unsafe (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

cons :: ListLike text char => ListLike list (Nontrivial text char) => text -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine

length :: ListLike text char => Nontrivial text char -> Positive Natural
length = Maybe.fromJust . preview Positive.natPrism . fromIntegral . ListLike.length . Nontrivial.generalize

lengthNat :: ListLike text char => Nontrivial text char -> Natural
lengthNat = fromIntegral . ListLike.length . Nontrivial.generalize

lengthInt :: ListLike text char => Nontrivial text char -> Integer
lengthInt = fromIntegral . ListLike.length . Nontrivial.generalize
