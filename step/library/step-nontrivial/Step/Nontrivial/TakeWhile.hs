{-# language Trustworthy #-}

module Step.Nontrivial.TakeWhile
  (
    takeWhile,
    TakeWhile (..),
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Type (Nontrivial)
import Step.Nontrivial.Refinement (generalize, refine)
import qualified Step.Nontrivial.Length as Nontrivial

import qualified ListLike
import qualified Positive
import qualified Maybe

data TakeWhile xs x =
    None
  | Prefix (Nontrivial xs x)
  | All

takeWhile :: ListLike xs x => (x -> Bool) -> Nontrivial xs x -> TakeWhile xs x
takeWhile f x =
    case refine (ListLike.takeWhile f (generalize x)) of
        Nothing -> None
        Just y -> if Nontrivial.length y == Nontrivial.length x then All else Prefix y
