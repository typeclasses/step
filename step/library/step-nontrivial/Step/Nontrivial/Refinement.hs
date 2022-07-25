{-# language Trustworthy #-}

module Step.Nontrivial.Refinement
  (
    refine,
    generalize,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Unsafe

import qualified ListLike
import qualified Maybe
import qualified Positive

refine :: ListLike xs x => xs -> Maybe (Nontrivial xs x)
refine x = if ListLike.null x then Nothing else Just (NontrivialUnsafe x)

generalize :: Nontrivial xs x -> xs
generalize (NontrivialUnsafe xs) = xs
