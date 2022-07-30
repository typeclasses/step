{-# language Trustworthy #-}

module Step.Internal.TupleIso
  (
    reassociateLeftToRight,
    associateLeft,
    associateRight,
  )
  where

import Optics

associateLeft :: Iso (a1, b1, c1) (a2, b2, c2) ((a1, b1), c1) ((a2, b2), c2)
associateLeft = iso (\(a, b, c) -> ((a, b), c)) (\((a, b), c) -> (a, b, c))

associateRight :: Iso (a1, b1, c1) (a2, b2, c2) (a1, (b1, c1)) (a2, (b2, c2))
associateRight = iso (\(a, b, c) -> (a, (b, c))) (\(a, (b, c)) -> (a, b, c))

reassociateLeftToRight :: Iso ((a1, b1), c1) ((a2, b2), c2) (a1, (b1, c1)) (a2, (b2, c2))
reassociateLeftToRight = re associateLeft % associateRight
