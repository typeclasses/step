module Step.Package.General
  (
    {- * Single characters -}
      peekChar, takeChar, satisfyJust, satisfyPredicate,
      nextCharIs, takeParticularChar,

    {- * Chunks -} peekSome, takeSome,

    {- * Particular text -}
      nextTextIs, takeParticularText, takeParticularTextAtomic,
      nextTextMatchesOn, takeMatchingText, takeMatchingTextAtomic,

    {- * Fixed-length -}
      trySkipPositive, skipPositive, trySkipNatural, skipNatural,
      takePositive, takePositiveAtomic,
      skipPositiveAtomic, skipNaturalAtomic,
      remainsAtLeastPositive, remainsAtLeastNatural,
      ensureAtLeastPositive, ensureAtLeastNatural,

    {- * End of input -} end, atEnd,

    {- * Failure -} fail, requireTrue, requireJust,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Interface (Step, Mode (..), AdvanceResult (..))
import Step.Walk (Walk (..))

import qualified Step.Do as P
import qualified Step.Interface as Interface
import qualified Step.Walk as Walk

import Control.Applicative (pure, (*>))
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (Factory, perform)
import Data.Functor.Contravariant

import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed
import qualified Optics

fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure getError

one :: Positive Natural
one = PositiveUnsafe 1

---

{-| Advance over the next /n/ characters (if possible)

    If the end of input is reached in fewer than /n/ characters,
    cursor advances to the end of input, and the returned
    'AdvanceResult' gives the size of the difference.
-}
trySkipPositive :: forall c m e. Positive Natural -> Sure c m e AdvanceResult
trySkipPositive n = Sure (Walk (Interface.commit n))

-- | Take a peek at the next character (if possible) without advancing
peekCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (One c))
peekCharMaybe = SureQuery (Walk Interface.peekCharMaybe)

---

trySkipNatural :: forall c m e. Natural -> Sure c m e AdvanceResult
trySkipNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure AdvanceSuccess
    Just p   ->  trySkipPositive p

skipPositive :: forall c m e. ErrorContext e m => Positive Natural -> Move c m e ()
skipPositive n = assumeMovement $ trySkipPositive n P.>>= \case
    AdvanceSuccess      ->  pure ()
    YouCanNotAdvance{}  ->  castTo @Any fail

skipNatural :: forall c m e. ErrorContext e m => Natural -> Any c m e ()
skipNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure ()
    Just p   ->  castTo @Any (skipPositive p)

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
nextCharIs c = act $ Interface.peekSomeMaybe <&> \case
    Just x | head x == c  ->  True
    _                     ->  False

takeParticularChar :: forall c m e. Chunk c => Eq (One c) => ErrorContext e m =>
    One c -> AtomicMove c m e ()
takeParticularChar c = assumeMovement $ Atom $ act $ Interface.peekSomeMaybe >>= \case
    Just x | head x == c  ->  pure $ Right $ act $ Interface.commit one $> ()
    _                     ->  perform getError <&> Left

peekSome :: forall c m e. ErrorContext e m => Query c m e c
peekSome = act $ Interface.peekSomeMaybe >>= \case
    Nothing  ->  perform getError <&> Left
    Just x   ->  pure (Right x)

takeSome :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e c
takeSome = assumeMovement $ Atom $ act $ Interface.peekSomeMaybe >>= \case
    Nothing  ->  perform getError <&> Left
    Just x   ->  pure $ Right $ trySkipPositive (length @c x) $> x

satisfyJust :: forall c m e a. Chunk c => ErrorContext e m =>
    (One c -> Maybe a) -> AtomicMove c m e a
satisfyJust ok = assumeMovement $ Atom $ act $ Interface.peekCharMaybe >>= \case
    Just (ok -> Just x)  ->  pure $ Right $ act $ Interface.commit one $> x
    _                    ->  perform getError <&> Left

satisfyPredicate :: forall c m e. Chunk c => ErrorContext e m =>
    (One c -> Bool) -> AtomicMove c m e (One c)
satisfyPredicate f = satisfyJust (\x -> if f x then Just x else Nothing)

remainsAtLeastPositive :: forall c m e. Chunk c =>
    Positive Natural -> SureQuery c m e Bool
remainsAtLeastPositive = \n -> act @SureQuery (go n)
  where
    go :: Positive Natural -> Factory (Step 'R c) m Bool
    go n = Interface.peekSomeMaybe >>= \case
        Nothing -> pure False
        Just x -> case Positive.minus n (length @c x) of
            Signed.Plus n' -> go n'
            _ -> pure True

remainsAtLeastNatural :: forall c m e. Chunk c =>
    Natural -> SureQuery c m e Bool
remainsAtLeastNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure True
    Just p   ->  remainsAtLeastPositive p

ensureAtLeastPositive :: forall c m e. Chunk c => ErrorContext e m =>
    Positive Natural -> Query c m e ()
ensureAtLeastPositive n = remainsAtLeastPositive n P.>>= \case
    False  ->  castTo @Query fail
    True   ->  pure ()

ensureAtLeastNatural :: forall c m e. Chunk c => ErrorContext e m =>
    Natural -> Query c m e ()
ensureAtLeastNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure ()
    Just p   ->  ensureAtLeastPositive p

skipPositiveAtomic :: forall c m e. Chunk c => ErrorContext e m =>
    Positive Natural -> AtomicMove c m e ()
skipPositiveAtomic n = assumeMovement $
    ensureAtLeastPositive n P.<* trySkipPositive n

skipNaturalAtomic :: forall c m e. Chunk c => ErrorContext e m =>
    Natural -> Atom c m e ()
skipNaturalAtomic n = case Optics.preview Positive.refine n of
    Nothing  ->  castTo @Atom (P.pure ())
    Just p   ->  castTo @Atom (skipPositiveAtomic p)

takePositive :: forall c m e. ErrorContext e m => Positive Natural -> Move c m e c
takePositive n = _

takePositiveAtomic :: forall c m e. Chunk c => ErrorContext e m =>
    Positive Natural -> AtomicMove c m e c
takePositiveAtomic = \n -> assumeMovement $ ensureAtLeastPositive n P.*> Sure (Walk (go n))
  where
    go :: Positive Natural -> Factory (Step 'RW c) m c
    go n = _

---

requireTrue :: forall c m e. ErrorContext e m => Bool -> Query c m e ()
requireTrue = \case
    True -> castTo @Query (P.pure ())
    False -> castTo @Query fail

requireJust :: forall c m e a. ErrorContext e m => Maybe a -> Query c m e a
requireJust = \case
    Just x -> castTo @Query (P.pure x)
    Nothing -> castTo @Query fail

---

atEnd :: SureQuery c m e Bool
atEnd = SureQuery (Walk Interface.atEnd)

end :: forall c m e. ErrorContext e m => Query c m e ()
end = atEnd P.>>= requireTrue

---

takeParticularText :: forall c m e. Chunk c => Eq c => ErrorContext e m => c -> Move c m e ()
takeParticularText = \t -> assumeMovement $
    Any (Walk (go t) <&> Right) P.>>= requireTrue
  where
    go :: c -> Factory (Step 'RW c) m Bool
    go t = Interface.peekSomeMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix (ChunkCharacterEquivalence (==)) x t of
            StripEitherPrefixFail           ->  pure False
            StripEitherPrefixAll            ->  Interface.commit (length t) <&> \_ -> True
            IsPrefixedBy{}                  ->  Interface.commit (length t) <&> \_ -> True
            IsPrefixOf{ afterPrefix = t' }  ->  Interface.commit (length x) *> go t'

nextTextIs :: forall c m e. Chunk c => Eq c => c -> SureQuery c m e Bool
nextTextIs = nextTextMatchesOn (ChunkCharacterEquivalence (==))

takeParticularTextAtomic :: forall c m e. Chunk c => Eq c =>
    ErrorContext e m => c -> AtomicMove c m e ()
takeParticularTextAtomic t = assumeMovement $
    (nextTextIs t P.>>= requireTrue) P.<* trySkipPositive (length t)

nextTextMatchesOn :: forall c m e. Chunk c =>
    ChunkCharacterEquivalence c -> c -> SureQuery c m e Bool
nextTextMatchesOn eq = \t -> SureQuery (Walk (go t))
  where
    go :: c -> Factory (Step mode c) m Bool
    go t = Interface.peekSomeMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail           ->  pure False
            StripEitherPrefixAll            ->  pure True
            IsPrefixedBy{}                  ->  pure True
            IsPrefixOf{ afterPrefix = t' }  ->  go t'

takeMatchingText :: forall c m e. Chunk c =>
    ChunkCharacterEquivalence c -> c -> Move c m e c
takeMatchingText eq t = _

takeMatchingTextAtomic :: forall c m e. Chunk c => ErrorContext e m =>
    ChunkCharacterEquivalence c -> c -> Move c m e c
takeMatchingTextAtomic eq t =
    (nextTextMatchesOn eq t P.>>= requireTrue) P.*> takePositive (length t)
