module Step.Package.Some where

import Step.Action.Core
import Chunk
import Step.Package.FixedLength
import Step.Interface

import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (maybe)
import SupplyChain (order)

peekSome :: forall c m r. Query c m r c
peekSome = act \r -> order next <&> \case{ End -> Left r; Item x -> Right x }

takeSome :: forall c m r. Chunk c => Atom c m r c
takeSome = Atom $ act \r -> order next <&> \case
    End -> Left r
    Item x -> Right $ trySkipPositive (length @c x) $> x
