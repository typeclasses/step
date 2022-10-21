module Step.Package.Some where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Package.FixedLength
import Step.Interface

import qualified Step.Interface as Interface

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (..))
import SupplyChain (perform)

import qualified SupplyChain

peekSome :: forall c m e. ErrorContext e m => Query c m e c
peekSome = act $ SupplyChain.order StepNext >>= \case
    Nothing  ->  perform getError <&> Left
    Just x   ->  pure (Right x)

takeSome :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e c
takeSome = assumeMovement $ Atom $ act $ SupplyChain.order StepNext >>= \case
    Nothing  ->  perform getError <&> Left
    Just x   ->  pure $ Right $ trySkipPositive (length @c x) $> x
