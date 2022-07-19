module Step.Nontrivial.SplitAt where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

import Step.Nontrivial.Span

splitAt :: ListLike text char => Natural -> Nontrivial text char -> Span text char
splitAt n whole = tupleSpan $ ListLike.splitAt (fromIntegral n) (Nontrivial.generalize whole)
