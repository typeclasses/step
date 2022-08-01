{-# language Trustworthy #-}

module Step.Nontrivial.SplitAtPositive
  (
    splitAtPositive,
    SplitAtPositive (..),
  )
  where

import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Type (Nontrivial)
import Step.Nontrivial.Unsafe (nontrivialUnsafe)
import Step.Nontrivial.Refinement (generalize)
import qualified Step.Nontrivial.Length as Nontrivial

import qualified ListLike
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed as Signed

data SplitAtPositive xs x =
    All
  | Insufficient (Positive Natural)
  | Split (Nontrivial xs x) (Nontrivial xs x)

splitAtPositive :: ListLike xs x => Positive Natural -> Nontrivial xs x -> SplitAtPositive xs x
splitAtPositive n whole =
    case Positive.minus (Nontrivial.length whole) n of
        Signed.Zero -> All
        Signed.Plus _ ->
            let (a, b) = ListLike.splitAt (fromIntegral (review Positive.refine n)) (generalize whole)
            in Split (nontrivialUnsafe a) (nontrivialUnsafe b)
        Signed.Minus n' ->
            Insufficient n'
