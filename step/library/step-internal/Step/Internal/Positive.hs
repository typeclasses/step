module Step.Internal.Positive where

import BasePrelude (Num, Ord)
import Optics (Prism')

import qualified Positive
import Positive (Positive)

positive :: (Num n, Ord n) => Prism' n (Positive n)
positive = Positive.refine
