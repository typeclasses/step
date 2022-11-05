module Step.Package.Some where

import Step.Action.Core
import Step.Chunk
import Step.Package.FixedLength
import Step.Interface

import qualified Step.Interface as Interface

import Control.Monad ((>>=))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (..), maybe)
import SupplyChain (perform, order, (>->))

import qualified SupplyChain

peekSome :: forall c m r. Query c m r r c
peekSome = act \r -> order nextMaybe <&> maybe (Left r) Right

takeSome :: forall c m r. Chunk c => AtomicMove c m r r c
takeSome = assumeMovement $ Atom $ act \r -> order nextMaybe <&> maybe (Left r) \x ->
    Right $ trySkipPositive (length @c x) $> x
