module Step.Action
  (
    {- * Actions -} Action, {- $types -}
    Any, any, runAny,
    Query, query, runQuery,
    Sure, sure, runSure,
    SureQuery, sureQuery, runSureQuery,
    Atom (..), Move, AtomicMove, Failure (..),

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

any :: ExceptT e (Factory (Step 'RW c) m) a -> Any c m e a
any (ExceptT a) = Any (ExceptT (Walk a))

query :: ExceptT e (Factory (Step 'R c) m) a -> Query c m e a
query (ExceptT a) = Query (ExceptT (Walk a))

sure :: Factory (Step 'RW c) m a -> Sure c m e a
sure a = Sure (Walk a)

sureQuery :: Factory (Step 'R c) m a -> SureQuery c m e a
sureQuery a = SureQuery (Walk a)

---

runAny :: Is p Any => p c m e a -> ExceptT e (Factory (Step 'RW c) m) a
runAny x = case castTo @Any x of Any (ExceptT (Walk y)) -> ExceptT y

runSure :: Is p Sure => p c m e a -> Factory (Step 'RW c) m a
runSure x = case castTo @Sure x of Sure (Walk y) -> y

runQuery :: Is p Query => p c m e a -> ExceptT e (Factory (Step 'R c) m) a
runQuery x = case castTo @Query x of Query (ExceptT (Walk y)) -> ExceptT y

runSureQuery :: Is p SureQuery => p c m e a -> Factory (Step 'R c) m a
runSureQuery x = case castTo @SureQuery x of SureQuery (Walk y) -> y

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
