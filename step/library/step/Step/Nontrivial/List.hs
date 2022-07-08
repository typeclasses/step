{-# language FlexibleContexts, Safe #-}

module Step.Nontrivial.List where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

cons :: ListLike a c => ListLike list (Nontrivial a) => a -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine
