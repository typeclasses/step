module Step.Action.Lift where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import Step.Action.Functor (FunctorAction)

import qualified Variado.Monad.Class as V

import qualified Monad

import Step.Action.KindJoin

import Step.Action.UnifiedType (ActionIso, actionIso, Action)

class ActionLift (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionLift :: Monad m => Action k1 config cursor error m a -> Action k2 config cursor error m a

-- todo: all instances, less than 64

instance ActionLift T.Any        T.Any        where actionLift = coerce
instance ActionLift T.Static     T.Static     where actionLift = coerce
instance ActionLift T.Move       T.Move       where actionLift = coerce
instance ActionLift T.Undo       T.Undo       where actionLift = coerce
instance ActionLift T.MoveUndo   T.MoveUndo   where actionLift = coerce
instance ActionLift T.Sure       T.Sure       where actionLift = coerce
instance ActionLift T.SureStatic T.SureStatic where actionLift = coerce
instance ActionLift T.SureMove   T.SureMove   where actionLift = coerce

instance ActionLift T.Move       T.Any  where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.MoveUndo   T.Any  where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.MoveUndo   T.Move where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.Sure       T.Any  where actionLift = view actionIso . Coerce.from @T.Any . T.sureToAny . Coerce.to @T.Sure . review actionIso
instance ActionLift T.SureStatic T.Any  where actionLift = view actionIso . Coerce.from @T.Any . T.sureToAny . Coerce.to @T.Sure . review actionIso
instance ActionLift T.SureStatic T.Sure where actionLift = view actionIso . coerce . review actionIso

actionLiftTo :: forall k2 k1 config cursor error m a.
    Monad m =>
    ActionLift k1 k2 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m a
actionLiftTo = actionLift @k1 @k2
