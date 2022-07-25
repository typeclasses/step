module Step.Nontrivial.SplitAt
  (
    splitAt, SplitAt (..),
  )
  where

import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Unsafe (Nontrivial (..))

import qualified Positive

import qualified Step.Nontrivial.SplitAtPositive as P
import Step.Nontrivial.SplitAtPositive (SplitAtPositive)

data SplitAt xs x = None | All | Insufficient (Positive Natural) | Split (Nontrivial xs x) (Nontrivial xs x)

splitAt :: ListLike xs x => Natural -> Nontrivial xs x -> SplitAt xs x
splitAt n = case preview Positive.refine n of
    Nothing -> \_ -> None
    Just p -> liftSplitAtPositive . P.splitAtPositive p

liftSplitAtPositive :: SplitAtPositive xs x -> SplitAt xs x
liftSplitAtPositive = \case
    P.All -> All
    P.Insufficient r -> Insufficient r
    P.Split a b -> Split a b
