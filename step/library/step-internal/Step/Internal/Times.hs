module Step.Internal.Times where

import BasePrelude
import qualified Semigroup
import Natural (Natural)

times :: Monoid a => Natural -> a -> a
times = Semigroup.mtimesDefault
