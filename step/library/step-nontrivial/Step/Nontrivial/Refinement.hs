{-# language Trustworthy #-}

module Step.Nontrivial.Refinement
  (
    refine,
    generalize,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Constructor

import qualified ListLike
import qualified Maybe
import qualified Positive
