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
import Step.Nontrivial.Constructor (generalize, length)

import qualified ListLike
import qualified Maybe
import qualified Positive

lengthNat :: Nontrivial xs x -> Natural
lengthNat = review Positive.refine . length

lengthInt :: ListLike xs x => Nontrivial xs x -> Integer
lengthInt = fromIntegral . lengthNat
