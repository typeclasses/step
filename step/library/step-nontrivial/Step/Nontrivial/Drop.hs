{-# language Trustworthy #-}

module Step.Nontrivial.Drop
  (
    drop,
    dropNat,
    Drop (..),
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Type (Nontrivial)
import Step.Nontrivial.Unsafe (nontrivialUnsafe)
import Step.Nontrivial.Constructor (length)
import Step.Nontrivial.Refinement (generalize)
import qualified Step.Nontrivial.Length as Nontrivial

import qualified ListLike
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed as Signed

dropNat :: ListLike xs x => Natural -> Nontrivial xs x -> Drop xs x
dropNat = maybe DroppedPart drop . preview Positive.refine

data Drop xs x =
    DroppedAll
  | InsufficientToDrop{ dropShortfall :: Positive Natural }
  | DroppedPart{ dropRemainder :: Nontrivial xs x }

drop :: ListLike xs x => Positive Natural -> Nontrivial xs x -> Drop xs x
drop n whole =
    case Positive.minus (length whole) n of
        Signed.Zero -> DroppedAll
        Signed.Plus _ -> DroppedPart{ dropRemainder = nontrivialUnsafe $
            ListLike.drop (fromIntegral (review Positive.refine n)) (generalize whole) }
        Signed.Minus dropShortfall -> InsufficientToDrop{ dropShortfall }
