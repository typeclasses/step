{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.List where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

cons :: ListLike a c => ListLike list (Nontrivial a) => a -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine

data Span text =
    All (Nontrivial text)
  | None (Nontrivial text)
  | Split (Nontrivial text) (Nontrivial text)

span :: ListLike text char => (char -> Bool) -> Nontrivial text -> Span text
span f whole =
    let (a, b) = ListLike.span f (Nontrivial.generalize whole) in
    if ListLike.null b then All (NontrivialUnsafe a) else
    if ListLike.null a then None (NontrivialUnsafe b) else
    Split (NontrivialUnsafe a) (NontrivialUnsafe b)
