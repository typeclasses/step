module Step.Action
  (
    {- * Actions -} Action, {- $types -}
    Any, Query, Sure, SureQuery,
    Atom (..), Move, AtomicMove,
    Failure (..),

    {- * Classes -} Atomic (..), AssumeMovement (..), Run (..), Act (..),

    {- * Subtyping (Is, castTo) -} {- $subtyping -} Is (..), castTo,

    {- * Composition -}
    {- ** Type family (>>) -} type (>>),
    {- ** Monad-like operations -} {- $do -} Join (..), bindAction,

    {- * Some actions -}
    {- ** Single characters -} peekCharMaybe, peekChar, takeChar, takeCharMaybe,
    {- ** Chunks -} peekSome, takeSome,
    {- ** Failure -} fail,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Interface (Walk (..), Step, Mode (..))

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Applicative (pure, (<*))
import Control.Monad ((>>=), when)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (($>), void, (<&>), fmap)
import Data.Maybe (Maybe (..), isJust)
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (Factory, perform)
import Control.Monad.Except (ExceptT (..))

import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

commit :: forall c m e. Positive Natural -> Sure c m e Interface.AdvanceResult
commit n = act $ Interface.commit n

fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure getError

one :: Positive Natural
one = PositiveUnsafe 1

peekCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (OneOf c))
peekCharMaybe = act $ Interface.nextMaybe <&> fmap @Maybe head

peekChar :: forall c m e. Chunk c => ErrorContext e m => Query c m e (OneOf c)
peekChar = peekCharMaybe P.>>= \case
    Nothing -> castTo @Query fail
    Just x  -> pure x

takeCharMaybe :: forall c m e. Chunk c => Sure c m e (Maybe (OneOf c))
takeCharMaybe = act do
    xm <- Interface.nextMaybe <&> fmap @Maybe head
    when (isJust xm) $ void $ Interface.commit one
    pure xm

takeChar :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e (OneOf c)
takeChar = assumeMovement $ peekCharMaybe P.>>= \case
    Nothing -> castTo @Atom fail
    Just x  -> castTo @Atom (commit one) $> x

peekSome :: forall c m e. ErrorContext e m => Query c m e c
peekSome = act $ Interface.nextMaybe >>= \case
    Nothing -> perform getError <&> Left
    Just x  -> pure (Right x)

takeSome :: forall c e m. Chunk c => ErrorContext e m => AtomicMove c m e c
takeSome = assumeMovement $ Atom $ act $ Interface.nextMaybe >>= \case
    Nothing -> perform getError <&> Left
    Just x  -> pure $ Right $ commit (length @c x) $> x

{- $do

For additional Monad-like operations, see "Step.Do" and consider using the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}
