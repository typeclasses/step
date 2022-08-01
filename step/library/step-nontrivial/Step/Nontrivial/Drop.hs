{-# language Trustworthy #-}

module Step.Nontrivial.Drop
  (
    drop,
    dropNat,
    Drop (..),
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Type (Nontrivial)
import Step.Nontrivial.Unsafe (nontrivialUnsafe)
import Step.Nontrivial.Refinement (generalize)
import qualified Step.Nontrivial.Length as Nontrivial
import Step.Nontrivial.Constructor (Drop (..), drop)

import qualified ListLike
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed as Signed

dropNat :: ListLike xs x => Natural -> Nontrivial xs x -> Drop xs x
dropNat = maybe DroppedPart (\n x -> drop x n) . preview Positive.refine
