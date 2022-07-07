module Step.Action.Subtyping
  (
    Is,
    cast,
  )
  where

import Step.Internal.Prelude

import Step.Action.Kinds

import qualified Step.Action.Coerce as Coerce

class Is (k1 :: ActionKind) (k2 :: ActionKind)
  where
    cast' :: Monad m => k1 config cursor error m a -> k2 config cursor error m a

cast :: forall k2 k1 config cursor error m a.
    Monad m =>
    Is k1 k2 =>
    k1 config cursor error m a
    -> k2 config cursor error m a
cast = cast' @k1 @k2

sureToAny :: Functor m => Sure config cursor error m a -> Any config cursor error m a
sureToAny (Sure p) = Any (\c -> p c <&> Right)


--- Identity

-- | Everything trivially lifts to itself
instance Is Any Any where cast' = coerce

-- | Everything trivially lifts to itself
instance Is Query Query where cast' = coerce

-- | Everything trivially lifts to itself
instance Is Move Move where cast' = coerce

-- | Everything trivially lifts to itself
instance Is Atom Atom where cast' = coerce

-- | Everything trivially lifts to itself
instance Is AtomicMove AtomicMove where cast' = coerce

-- | Everything trivially lifts to itself
instance Is Sure Sure where cast' = coerce

-- | Everything trivially lifts to itself
instance Is SureQuery SureQuery where cast' = coerce


--- Any supertypes everything else

-- | Everything lifts to Any
instance Is Query Any where cast' = coerce

-- | Everything lifts to Any
instance Is Move Any where cast' = coerce

-- | Everything lifts to Any
instance Is AtomicMove Any where cast' = coerce

-- | Everything lifts to Any
instance Is Sure Any where cast' = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- | Everything lifts to Any
instance Is SureQuery Any where cast' = Coerce.from @Any . sureToAny . Coerce.to @Sure

-- | Everything lifts to Any
instance Is Atom Any where cast' = coerce


--- Atom + Move = AtomicMove

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Move where cast' = coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Atom where cast' = coerce


--- Sure + Query = SureQuery

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Sure where cast' = coerce

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Query where cast' = Coerce.from @Any . sureToAny . Coerce.to @Sure


--- Trivial subtypes of Atom

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance Is Query Atom where cast' = coerce

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance Is Sure Atom where cast' = Coerce.from @Any . sureToAny . Coerce.to @Sure
