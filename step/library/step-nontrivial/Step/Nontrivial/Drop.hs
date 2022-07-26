{-# language Trustworthy #-}

module Step.Nontrivial.Drop
  (
    drop, dropPositive,
    Drop (..),
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Unsafe (Nontrivial (..))
import Step.Nontrivial.Refinement (generalize)
import qualified Step.Nontrivial.Length as Nontrivial

import qualified ListLike
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed as Signed

data Drop xs x =
    DroppedAll
  | Insufficient{ shortfall :: Positive Natural }
  | DroppedPart{ remainder :: Nontrivial xs x }

drop :: ListLike xs x => Natural -> Nontrivial xs x -> Drop xs x
drop = maybe DroppedPart dropPositive . preview Positive.refine

dropPositive :: ListLike xs x => Positive Natural -> Nontrivial xs x -> Drop xs x
dropPositive n whole =
    case Positive.minus (Nontrivial.length whole) n of
        Signed.Zero -> DroppedAll
        Signed.Plus _ -> DroppedPart{ remainder = NontrivialUnsafe $
            ListLike.drop (fromIntegral (review Positive.refine n)) (generalize whole) }
        Signed.Minus shortfall -> Insufficient{ shortfall }
