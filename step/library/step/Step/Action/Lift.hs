module Step.Action.Lift where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import qualified Monad

import Step.Action.KindJoin

class ActionLift (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionLift :: Monad m => k1 config cursor error m a -> k2 config cursor error m a

-- todo: all instances, less than 64

instance ActionLift Any        Any        where actionLift = coerce
instance ActionLift Static     Static     where actionLift = coerce
instance ActionLift Move       Move       where actionLift = coerce
instance ActionLift Undo       Undo       where actionLift = coerce
instance ActionLift MoveUndo   MoveUndo   where actionLift = coerce
instance ActionLift Sure       Sure       where actionLift = coerce
instance ActionLift SureStatic SureStatic where actionLift = coerce
instance ActionLift SureMove   SureMove   where actionLift = coerce

instance ActionLift Move       Any  where actionLift = coerce
instance ActionLift MoveUndo   Any  where actionLift = coerce
instance ActionLift MoveUndo   Move where actionLift = coerce
instance ActionLift Sure       Any  where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
instance ActionLift SureStatic Any  where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
instance ActionLift SureStatic Sure where actionLift = coerce
instance ActionLift Undo       Any  where actionLift = coerce

actionLiftTo :: forall k2 k1 config cursor error m a.
    Monad m =>
    ActionLift k1 k2 =>
    k1 config cursor error m a
    -> k2 config cursor error m a
actionLiftTo = actionLift @k1 @k2

sureToAny :: Functor m => Sure config cursor error m a -> Any config cursor error m a
sureToAny (Sure p) = Any (\c -> p c <&> Right)
