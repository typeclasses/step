module Step.Nontrivial.Misc where

import Step.Internal.Prelude

import Step.Nontrivial.Type

import qualified Positive

lengthNat :: Nontrivial xs x -> Natural
lengthNat = review Positive.refine . length

lengthInt :: Nontrivial xs x -> Integer
lengthInt = fromIntegral . lengthNat
