{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.List where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

cons :: ListLike text char => ListLike list (Nontrivial text char) => text -> list -> list
cons = maybe id Prelude.cons . Nontrivial.refine

data Span text char = All | None | Split (Nontrivial text char) (Nontrivial text char)

tupleSpan :: ListLike text char => (text, text) -> Span text char
tupleSpan (a, b) =
    if ListLike.null b then All else
    if ListLike.null a then None else
    Split (NontrivialUnsafe a) (NontrivialUnsafe b)

span :: ListLike text char => (char -> Bool) -> Nontrivial text char -> Span text char
span f whole = tupleSpan $ ListLike.span f (Nontrivial.generalize whole)

splitAt :: ListLike text char => Natural -> Nontrivial text char -> Span text char
splitAt n whole = tupleSpan $ ListLike.splitAt (fromIntegral n) (Nontrivial.generalize whole)

length :: ListLike text char => Nontrivial text char -> Natural
length = fromIntegral . ListLike.length . Nontrivial.generalize

takeWhile :: ListLike text char => (char -> Bool) -> Nontrivial text char -> Maybe (Nontrivial text char)
takeWhile f = Nontrivial.refine . ListLike.takeWhile f . Nontrivial.generalize
