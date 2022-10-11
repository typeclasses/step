module Step.Action
  (
    {- * Actions -} Action, {- $types -}
    {- ** Any -} Any, any,
    {- ** Query -} Query, query,
    {- ** Sure -} Sure, sure,
    {- ** SureQuery -} SureQuery, sureQuery,
    {- ** Atom -} Atom (..),
    {- ** Move -} Move,
    {- ** AtomicMove -} AtomicMove,
    {- ** Failure -} Failure (..),

    {- * Classes -} Atomic (..), AssumeMovement (..), Run (..),

    {- * Subtyping -} {- $subtyping -} Is (..), castTo,

    {- * Composition -} type (>>), Join (..), bindAction,

    {- * Some actions -} nextCharMaybe, takeChar, takeCharMaybe
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
import Data.Functor (($>), void)
import Data.Maybe (Maybe (..), isJust)
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (Factory)
import Control.Monad.Except (ExceptT (..))

import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

---

any :: Factory (Step 'RW c) m (Either e a) -> Any c m e a
any = Any . Walk

query :: Factory (Step 'R c) m (Either e a) -> Query c m e a
query = Query . Walk

sure :: Factory (Step 'RW c) m a -> Sure c m e a
sure = Sure . Walk

sureQuery :: Factory (Step 'R c) m a -> SureQuery c m e a
sureQuery = SureQuery . Walk

---

nextCharMaybe :: forall c m e. Chunk c => SureQuery c m e (Maybe (OneOf c))
nextCharMaybe = sureQuery Interface.nextCharMaybe

takeCharMaybe :: forall c m e. Chunk c => Sure c m e (Maybe (OneOf c))
takeCharMaybe = sure do
    xm <- Interface.nextCharMaybe
    when (isJust xm) $ void $ Interface.commit one
    pure xm

takeChar :: forall c m e. Chunk c => ErrorContext e m => AtomicMove c m e (OneOf c)
takeChar = assumeMovement $ nextCharMaybe P.>>= \case
    Nothing -> castTo @Atom fail
    Just x  -> castTo @Atom (trySkip one) $> x

trySkip :: Positive Natural -> Sure c m e Interface.AdvanceResult
trySkip n = sure $ Interface.commit n

fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure getError

one :: Positive Natural
one = PositiveUnsafe 1
