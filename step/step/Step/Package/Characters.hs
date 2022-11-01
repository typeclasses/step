module Step.Package.Characters
  (
    peekChar, takeChar, satisfyJust, satisfyPredicate,
    nextCharIs, takeParticularChar,
  )
  where

import Step.Action.Core
import Step.Chunk
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
peekCharMaybe :: forall c m r. Chunk c => SureQuery c m r r (Maybe (One c))
peekCharMaybe = SureQuery $ ResettingSequence $ order nextMaybe <&> fmap @Maybe head

peekChar :: forall c m r. Chunk c => Query c m r r (One c)
peekChar = peekCharMaybe P.>>= \case
    Nothing  ->  castTo @Query fail
    Just x   ->  pure x

-- | Advance over the next character and return it; fail if end of input
takeChar :: forall c m r. Chunk c => AtomicMove c m r r (One c)
takeChar = assumeMovement $ peekCharMaybe P.>>= \case
    Nothing  ->  castTo @Atom fail
    Just x   ->  castTo @Atom (trySkipPositive one) $> x

nextCharIs :: forall c m r. Chunk c => Eq (One c) => One c -> SureQuery c m r r Bool
nextCharIs c = act $ order nextMaybe <&> \case
    Just x | head x == c  ->  True
    _                     ->  False

takeParticularChar :: forall c m r. Chunk c => Eq (One c) =>
    One c -> AtomicMove c m r r ()
takeParticularChar c = assumeMovement $ Atom $ act $ order nextMaybe >>= \case
    Just x | head x == c  ->  pure $ Right $ act $ order (commit one) $> ()
    _                     ->  SupplyChain.param <&> Left

satisfyJust :: forall c m r a. Chunk c =>
    (One c -> Maybe a) -> AtomicMove c m r r a
satisfyJust ok = assumeMovement $ Atom $ act $ order nextMaybe <&> fmap @Maybe head >>= \case
    Just (ok -> Just x)  ->  pure $ Right $ act $ order (commit one) $> x
    _                    ->  SupplyChain.param <&> Left

satisfyPredicate :: forall c m r. Chunk c =>
    (One c -> Bool) -> AtomicMove c m r r (One c)
satisfyPredicate f = satisfyJust (\x -> if f x then Just x else Nothing)

one :: Positive Natural
one = PositiveUnsafe 1
