{-# language Trustworthy #-}

module Step.Nontrivial.Length
  (
    length,
    lengthNat,
    lengthInt,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Type (Nontrivial)
import Step.Nontrivial.Refinement (generalize)

import qualified ListLike
import qualified Maybe
import qualified Positive

length :: ListLike xs x => Nontrivial xs x -> Positive Natural
length = Maybe.fromJust . preview Positive.natPrism . fromIntegral . ListLike.length . generalize

lengthNat :: ListLike xs x => Nontrivial xs x -> Natural
lengthNat = fromIntegral . ListLike.length . generalize

lengthInt :: ListLike xs x => Nontrivial xs x -> Integer
lengthInt = fromIntegral . ListLike.length . generalize
