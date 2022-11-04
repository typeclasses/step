module Step.Package.Some where

import Step.Action.Core
import Step.Chunk
import Step.Package.FixedLength
import Step.Interface

import qualified Step.Interface as Interface

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (..))
import SupplyChain (perform, order, absurdOrder, (>->))

import qualified SupplyChain

peekSome :: forall c m r. Query c m r r c
peekSome = act \r -> order nextMaybe <&> \case
    Nothing  ->  Left r
    Just x   ->  Right x

takeSome :: forall c m r. Chunk c => AtomicMove c m r r c
takeSome = assumeMovement $ Atom $ act \r -> order nextMaybe <&> \case
    Nothing  ->  Left r
    Just x   ->  Right $ trySkipPositive (length @c x) $> x
