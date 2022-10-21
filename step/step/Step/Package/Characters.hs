module Step.Package.Characters
  (
    peekChar, takeChar, satisfyJust, satisfyPredicate,
    nextCharIs, takeParticularChar,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Package.FixedLength
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor (($>), (<&>), fmap)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (perform, order)

import qualified SupplyChain

-- | Take a peek at the next character (if possible) without advancing
peekCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (One c))
peekCharMaybe = SureQuery $ ResettingSequence $ order nextMaybe <&> fmap @Maybe head

peekChar :: forall c m e. Chunk c => ErrorContext e m => Query c m e (One c)
peekChar = peekCharMaybe P.>>= \case
    Nothing  ->  castTo @Query fail
    Just x   ->  pure x

-- | Advance over the next character and return it; fail if end of input
takeChar :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e (One c)
takeChar = assumeMovement $ peekCharMaybe P.>>= \case
    Nothing  ->  castTo @Atom fail
    Just x   ->  castTo @Atom (trySkipPositive one) $> x

nextCharIs :: forall c m e. Chunk c => Eq (One c) => One c -> SureQuery c m e Bool
nextCharIs c = act $ order nextMaybe <&> \case
    Just x | head x == c  ->  True
    _                     ->  False

takeParticularChar :: forall c m e. Chunk c => Eq (One c) => ErrorContext e m =>
    One c -> AtomicMove c m e ()
takeParticularChar c = assumeMovement $ Atom $ act $ order nextMaybe >>= \case
    Just x | head x == c  ->  pure $ Right $ act $ order (commit one) $> ()
    _                     ->  perform getError <&> Left

satisfyJust :: forall c m e a. Chunk c => ErrorContext e m =>
    (One c -> Maybe a) -> AtomicMove c m e a
satisfyJust ok = assumeMovement $ Atom $ act $ order nextMaybe <&> fmap @Maybe head >>= \case
    Just (ok -> Just x)  ->  pure $ Right $ act $ order (commit one) $> x
    _                    ->  perform getError <&> Left

satisfyPredicate :: forall c m e. Chunk c => ErrorContext e m =>
    (One c -> Bool) -> AtomicMove c m e (One c)
satisfyPredicate f = satisfyJust (\x -> if f x then Just x else Nothing)

one :: Positive Natural
one = PositiveUnsafe 1
