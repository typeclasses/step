module Step.Action
  (
    {- * Actions -} Action, Walk (..), {- $types -}
    Any (..), Query (..), Sure (..), SureQuery (..),
    Atom (..), Move (..), AtomicMove (..), Failure (..),

    {- * Classes -}
    Trivial (..), Fallible (..), Atomic (..), AssumeMovement (..),

    {- * Subtyping -} {- $subtyping -} Is (..), castTo,

    {- * Composition -} type (>>), Join (..), bindAction,

    {- * Some actions -} nextCharMaybe, takeChar, takeCharMaybe
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Applicative (pure, (<*))
import Control.Monad ((>>=))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>))
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))

import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

nextCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (OneOf c))
nextCharMaybe = SureQuery $ Walk Interface.nextCharMaybe

takeCharMaybe :: forall c m e. Chunk c => Sure c m e (Maybe (OneOf c))
takeCharMaybe = Sure $ Walk $ Interface.nextCharMaybe >>= \case
    Nothing -> pure Nothing
    Just x  -> Interface.commit one $> Just x

takeChar :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e (OneOf c)
takeChar = assumeMovement $ nextCharMaybe P.>>= \case
    Nothing -> castTo @Atom fail
    Just x  -> castTo @Atom (trySkip one) $> x

trySkip :: Positive Natural -> Sure c m e Interface.AdvanceResult
trySkip n = Sure $ Walk $ Interface.commit n

fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure getError

one :: Positive Natural
one = PositiveUnsafe 1
