module Step.Nontrivial.List where

import Step.Internal.Prelude

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

cons :: ListLike a c => ListLike list (Nontrivial a) => a -> list -> list
cons = maybe id ListLike.cons . Nontrivial.refine
