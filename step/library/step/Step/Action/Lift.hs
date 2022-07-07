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

-- | Everything trivially lifts to itself
instance ActionLift Any Any where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift Query Query where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift Move Move where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift Atom Atom where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift AtomicMove AtomicMove where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift Sure Sure where actionLift = coerce

-- | Everything trivially lifts to itself
instance ActionLift SureQuery SureQuery where actionLift = coerce

-- | Everything lifts to Any
instance ActionLift Query Any where actionLift = coerce

-- | Everything lifts to Any
instance ActionLift Move Any where actionLift = coerce

-- | Everything lifts to Any
instance ActionLift AtomicMove Any where actionLift = coerce

-- | Everything lifts to Any
instance ActionLift Sure Any where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- | Everything lifts to Any
instance ActionLift SureQuery Any where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- | Everything lifts to Any
instance ActionLift Atom Any where actionLift = coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance ActionLift AtomicMove Move where actionLift = coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance ActionLift AtomicMove Atom where actionLift = coerce

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance ActionLift SureQuery Sure where actionLift = coerce

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance ActionLift SureQuery Query where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance ActionLift Query Atom where actionLift = coerce

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance ActionLift Sure Atom where actionLift = Coerce.from @Any . sureToAny . Coerce.to @Sure
