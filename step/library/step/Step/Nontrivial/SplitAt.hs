module Step.Nontrivial.SplitAt
  (
    splitAt, SplitAt (..),
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

import qualified Step.Nontrivial.SplitAtPositive as P
import Step.Nontrivial.SplitAtPositive (splitAtPositive, SplitAtPositive)

data SplitAt text char = None | All | Insufficient (Positive Natural) | Split (Nontrivial text char) (Nontrivial text char)

splitAt :: ListLike text char => Natural -> Nontrivial text char -> SplitAt text char
splitAt n = case preview Positive.refine n of
    Nothing -> \_ -> None
    Just p -> liftSplitAtPositive . P.splitAtPositive p

liftSplitAtPositive :: SplitAtPositive text char -> SplitAt text char
liftSplitAtPositive = \case
    P.All -> All
    P.Insufficient r -> Insufficient r
    P.Split a b -> Split a b
