module Step.Action.Lift where

import Step.Internal.Prelude

import Step.Action.Kinds

import qualified Step.Action.Coerce as Coerce

class ActionLift (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionLift :: Monad m => k1 config cursor error m a -> k2 config cursor error m a

actionLiftTo :: forall k2 k1 config cursor error m a.
    Monad m =>
    ActionLift k1 k2 =>
    k1 config cursor error m a
    -> k2 config cursor error m a
actionLiftTo = actionLift @k1 @k2

sureToAny :: Functor m => Sure config cursor error m a -> Any config cursor error m a
sureToAny (Sure p) = Any (\c -> p c <&> Right)

-- Everything trivially lifts to itself

instance ActionLift Any        Any        where actionLift = coerce
instance ActionLift Query      Query      where actionLift = coerce
instance ActionLift Move       Move       where actionLift = coerce
instance ActionLift Atom       Atom       where actionLift = coerce
instance ActionLift AtomicMove AtomicMove where actionLift = coerce
instance ActionLift Sure       Sure       where actionLift = coerce
instance ActionLift SureQuery  SureQuery  where actionLift = coerce

-- Everything lifts to Any

instance ActionLift Query      Any  where actionLift = coerce
instance ActionLift Move       Any  where actionLift = coerce
instance ActionLift AtomicMove Any  where actionLift = coerce
instance ActionLift Sure       Any  where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
instance ActionLift SureQuery  Any  where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
instance ActionLift Atom       Any  where actionLift = coerce

-- AtomicMove lifts to either Atom or Move

instance ActionLift AtomicMove Move where actionLift = coerce
instance ActionLift AtomicMove Atom where actionLift = coerce

-- SureQuery lifts to either Sure or Query

instance ActionLift SureQuery Sure  where actionLift = coerce
instance ActionLift SureQuery Query where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- Queries and Sure actions are trivially atomic

instance ActionLift Query Atom where actionLift = coerce
instance ActionLift Sure  Atom where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
