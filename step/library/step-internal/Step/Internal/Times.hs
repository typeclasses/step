module Step.Internal.Times where

import BasePrelude
import qualified Semigroup

times :: Integral n => Monoid a => n -> a -> a
times = Semigroup.mtimesDefault
