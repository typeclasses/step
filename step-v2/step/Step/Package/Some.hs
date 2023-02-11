module Step.Package.Some where

import Step.Action.Core
import Block.Class.Class
import Step.Package.FixedLength
import Step.Interface
import Essentials

import Data.Either (Either (..))
import SupplyChain (order)

peekSome :: forall c m r. Query c m r c
peekSome = act \r -> order next <&> \case{ End -> Left r; Item x -> Right x }

takeSome :: forall c m r. Block c => Atom c m r c
takeSome = Atom $ act \r -> order next <&> \case
    End -> Left r
    Item x -> Right $ trySkipPositive (length @c x) $> x
