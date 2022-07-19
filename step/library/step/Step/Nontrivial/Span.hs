module Step.Nontrivial.Span where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

data Span text char = All | None | Split (Nontrivial text char) (Nontrivial text char)

span :: ListLike text char => (char -> Bool) -> Nontrivial text char -> Span text char
span f whole = tupleSpan $ ListLike.span f (Nontrivial.generalize whole)

tupleSpan :: ListLike text char => (text, text) -> Span text char
tupleSpan (a, b) =
    if ListLike.null b then All else
    if ListLike.null a then None else
    Split (NontrivialUnsafe a) (NontrivialUnsafe b)
