module Step.Action
  (
    {- * Actions -} Action, {- $types -}
      Any, Query, Sure, SureQuery, Atom (..), Move, AtomicMove, Failure (..),

    {- * Classes -} Atomic (..), AssumeMovement (..), Run (..), Act (..),

    {- * Subtyping (Is, castTo) -} {- $subtyping -} Is (..), castTo,

    {- * Composition -}
    {- ** Type family (>>) -} type (>>),
    {- ** Monad-like operations -} {- $do -} Join (..), bindAction,

    {- * Some actions -}
    {- ** Single characters -}
      peekCharMaybe, peekChar, takeChar, takeCharMaybe, satisfyJust,
    {- ** Chunks -} peekSome, peekSomeMaybe, takeSome, takeSomeMaybe,
    {- ** Fixed-length -} trySkipPositive, skipPositive,
    {- ** Failure -} fail,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Interface (Walk (..), Step, Mode (..), AdvanceResult (..))

import qualified Step.Do as P
import qualified Step.Interface as Interface

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

fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure getError

one :: Positive Natural
one = PositiveUnsafe 1

---

trySkipPositive :: forall c m e. Positive Natural -> Sure c m e Interface.AdvanceResult
trySkipPositive n = act (Interface.commit n)

peekSomeMaybe :: forall c m e. ErrorContext e m => SureQuery c m e (Maybe c)
peekSomeMaybe = act Interface.peekSomeMaybe

peekCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (One c))
peekCharMaybe = act Interface.peekCharMaybe

---

skipPositive :: forall c e m. Chunk c => ErrorContext e m => Positive Natural -> Move c m e ()
skipPositive n = assumeMovement $ trySkipPositive n P.>>= \case
    AdvanceSuccess -> pure ()
    YouCanNotAdvance{} -> castTo @Any fail

peekChar :: forall c m e. Chunk c => ErrorContext e m => Query c m e (One c)
peekChar = peekCharMaybe P.>>= \case
    Nothing -> castTo @Query fail
    Just x  -> pure x

takeCharMaybe :: forall c m e. Chunk c => Sure c m e (Maybe (One c))
takeCharMaybe = act do
    xm <- Interface.peekCharMaybe
    _ <- for_ xm \_ -> Interface.commit one
    pure xm

takeChar :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e (One c)
takeChar = assumeMovement $ peekCharMaybe P.>>= \case
    Nothing -> castTo @Atom fail
    Just x  -> castTo @Atom (trySkipPositive one) $> x

peekSome :: forall c m e. ErrorContext e m => Query c m e c
peekSome = act $ Interface.peekSomeMaybe >>= \case
    Nothing -> perform getError <&> Left
    Just x  -> pure (Right x)

takeSome :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e c
takeSome = assumeMovement $ Atom $ act $ Interface.peekSomeMaybe >>= \case
    Nothing -> perform getError <&> Left
    Just x  -> pure $ Right $ trySkipPositive (length @c x) $> x

takeSomeMaybe :: forall c m e. Chunk c => Sure c m e (Maybe c)
takeSomeMaybe = act do
    xm <- Interface.peekSomeMaybe
    _ <- for_ xm \x -> Interface.commit (length @c x)
    pure xm

satisfyJust :: forall c m e a. Chunk c => ErrorContext e m => (One c -> Maybe a) -> AtomicMove c m e a
satisfyJust ok = assumeMovement $ Atom $ act $
    Interface.peekCharMaybe >>= \case
        Just (ok -> Just x) -> pure $ Right $ act $ Interface.commit one $> x
        _ -> perform getError <&> Left

-- skipPositive :: forall c e m. Chunk c => ErrorContext e m => Positive Natural -> Move c m e ()
-- skipPositive = \n -> assumeMovement $ act @Any $ go n
--   where
--     go :: Positive Natural -> Factory (Step 'RW c) m (Either e ())
--     go n = Interface.peekSomeMaybe >>= \case
--         Nothing -> perform getError <&> Left
--         Just x -> case Positive.minus n (length @c x) of
--             Signed.Plus n' -> Interface.commit (length @c x) *> go n'
--             _ -> Interface.commit n $> Right ()


{- $do

For additional Monad-like operations, see "Step.Do" and consider using the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}
