module Step.Action
  (
    {- * Actions -} Action, {- $types -}
      Any, Query, Sure, SureQuery, Atom (..), Move, AtomicMove, Failure (..),

    {- * Classes -} Atomic (..), AssumeMovement (..), IsWalk (..), run, act,

    {- * Subtyping (Is, castTo) -} {- $subtyping -} Is (..), castTo,

    {- * Composition -}
    {- ** Type family (>>) -} type (>>),
    {- ** Monad-like operations -} {- $do -} Join (..), bindAction,

    {- * Examples -}
    {- ** Single characters -} peekChar, takeChar, satisfyJust,
    {- ** Chunks -} peekSome, takeSome,
    {- ** Particular text -} nextTextIs, takeText, takeTextAtomic,
    {- ** Fixed-length -}
      trySkipPositive, skipPositive, trySkipNatural, skipNatural,
      skipPositiveAtomic, skipNaturalAtomic,
      remainsAtLeastPositive, remainsAtLeastNatural,
      ensureAtLeastPositive, ensureAtLeastNatural,
    {- ** End of input -} end, atEnd,
    {- ** Failure -} fail,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Interface (Step, Mode (..), AdvanceResult (..))

import qualified Step.Do as P
import qualified Step.Interface as Interface
import qualified Step.Walk as Walk

import Data.Bool (Bool (..))
import Control.Applicative (pure, (<*), (*>))
import Control.Monad ((>>=), when)
import Data.Either (Either (..))
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Functor (($>), void, (<&>), fmap)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (Factory, perform)
import Control.Monad.Except (ExceptT (..))

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
trySkipPositive n = Sure (Walk.commit n)

-- | Take a peek at the next character (if possible) without advancing
peekCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (One c))
peekCharMaybe = SureQuery Walk.peekCharMaybe

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

---

require :: forall c m e. ErrorContext e m => Bool -> Query c m e ()
require = \case
    True -> castTo @Query (P.pure ())
    False -> castTo @Query fail

---

atEnd :: SureQuery c m e Bool
atEnd = SureQuery Walk.atEnd

end :: forall c m e. ErrorContext e m => Query c m e ()
end = atEnd P.>>= require

---

takeText :: forall c m e. Chunk c => ErrorContext e m => c -> Move c m e ()
takeText t = assumeMovement $ Any (Walk.takeText t <&> Right) P.>>= require

nextTextIs :: forall c m e. Chunk c => c -> SureQuery c m e Bool
nextTextIs t = SureQuery (Walk.nextTextIs t)

takeTextAtomic :: forall c m e. Chunk c =>
    ErrorContext e m => c -> AtomicMove c m e ()
takeTextAtomic t = assumeMovement $
    (nextTextIs t P.>>= require) P.<* trySkipPositive (length t)

{- $do

For additional Monad-like operations, see "Step.Do" and consider using the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}
