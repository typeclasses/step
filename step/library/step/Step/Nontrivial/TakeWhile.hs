module Step.Nontrivial.TakeWhile
  (
    takeWhile,
    TakeWhile (..),
  )
  where

import qualified Step.Internal.Prelude as Prelude
import Step.Internal.Prelude hiding (cons)

import Step.Nontrivial.Constructor (Nontrivial (..))
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial

import qualified ListLike

import qualified Positive

import qualified Maybe

data TakeWhile text char = None | Prefix (Nontrivial text char) | All

takeWhile :: ListLike text char => (char -> Bool) -> Nontrivial text char -> TakeWhile text char
takeWhile f x =
    case Nontrivial.refine (ListLike.takeWhile f (Nontrivial.generalize x)) of
        Nothing -> None
        Just y -> if Nontrivial.length y == Nontrivial.length x then All else Prefix y
