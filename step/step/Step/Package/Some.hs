module Step.Package.Some where

import Step.Action.Core
import Step.Chunk
import Step.Package.FixedLength
import Step.Interface

import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (maybe)
import SupplyChain (order)

peekSome :: forall c m r. Query c m r r c
peekSome = act \r -> order nextMaybe <&> maybe (Left r) Right

takeSome :: forall c m r. Chunk c => AtomicMove c m r r c
takeSome = assumeMovement $ Atom $ act \r -> order nextMaybe <&> maybe (Left r) \x ->
    Right $ trySkipPositive (length @c x) $> x
