module Step.Nontrivial.Span where

import Step.Internal.Prelude

import Step.Nontrivial.Unsafe (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

data Span xs x = All | None | Split (Nontrivial xs x) (Nontrivial xs x)

span :: ListLike xs x => (x -> Bool) -> Nontrivial xs x -> Span xs x
span f whole = tupleSpan $ ListLike.span f (Nontrivial.generalize whole)

tupleSpan :: ListLike xs x => (xs, xs) -> Span xs x
tupleSpan (a, b) =
    if ListLike.null b then All else
    if ListLike.null a then None else
    Split (NontrivialUnsafe a) (NontrivialUnsafe b)
