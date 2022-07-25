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

cons :: ListLike xs x => ListLike list (Nontrivial xs x) => xs -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine

length :: ListLike xs x => Nontrivial xs x -> Positive Natural
length = Maybe.fromJust . preview Positive.natPrism . fromIntegral . ListLike.length . Nontrivial.generalize

lengthNat :: ListLike xs x => Nontrivial xs x -> Natural
lengthNat = fromIntegral . ListLike.length . Nontrivial.generalize

lengthInt :: ListLike xs x => Nontrivial xs x -> Integer
lengthInt = fromIntegral . ListLike.length . Nontrivial.generalize
