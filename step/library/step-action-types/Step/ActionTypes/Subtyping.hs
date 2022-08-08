{-# language DataKinds, InstanceSigs, KindSignatures, MultiParamTypeClasses #-}

module Step.ActionTypes.Subtyping
  (

    cast,

    Is,
    -- The class's method is not exported because there is no reason to define new instances of the class outside of this module.

  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.Fallible

import Coerce (coerce)


class Is (act1 :: Action) (act2 :: Action) where
    cast' :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a


cast :: forall act2 act1 xs x r s e m a. (Monad m, Is act1 act2) => act1 xs x r s e m a -> act2 xs x r s e m a
cast = cast' @act1 @act2


-- Identity

-- | Everything trivially lifts to itself
instance Is Any Any where cast' = id

-- | Everything trivially lifts to itself
instance Is Query Query where cast' = id

-- | Everything trivially lifts to itself
instance Is Move Move where cast' = id

-- | Everything trivially lifts to itself
instance Is Atom Atom where cast' = id

-- | Everything trivially lifts to itself
instance Is AtomicMove AtomicMove where cast' = id

-- | Everything trivially lifts to itself
instance Is Sure Sure where cast' = id

-- | Everything trivially lifts to itself
instance Is SureQuery SureQuery where cast' = id

-- | Everything trivially lifts to itself
instance Is Fail Fail where cast' = id


-- Any supertypes everything else

-- | Everything lifts to Any
instance Is Query Any where
    cast' = \case
        Query_Lift x -> Any_Lift x
        Query_Ask f -> Any_Ask f
        Query_Get f -> Any_Get f
        Query_Next f -> Any_Next f
        Query_Join x -> Any_Join (cast $ fmap cast x)
        Query_Fail f -> Any_Fail f

-- | Everything lifts to Any
instance Is Move Any where cast' = coerce

-- | Everything lifts to Any
instance Is AtomicMove Any where cast' = cast' @Atom @Any . coerce

-- | Everything lifts to Any
instance Is Sure Any where
    cast' :: forall xs x r s e m a. Monad m => Sure xs x r s e m a -> Any xs x r s e m a
    cast' (Sure x) = mapError absurd x

-- | Everything lifts to Any
instance Is SureQuery Any where
    cast' (SureQuery x) = cast (mapError absurd x)

-- | Everything lifts to Any
instance Is Atom Any where
    cast' (Atom q) = Any_Join (cast @Any (fmap (cast @Any) q))


-- Atom + Move = AtomicMove

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Move where
    cast' = coerce . cast' @Atom @Any . coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Atom where
    cast' = coerce


-- Sure + Query = SureQuery

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Sure where
  cast' :: forall xs x r s e m a. Monad m => SureQuery xs x r s e m a -> Sure xs x r s e m a
  cast' = go
    where
      go :: forall a'. SureQuery xs x r s e m a' -> Sure xs x r s e m a'
      go = \case
        SureQuery (Query_Lift x) -> Sure (Any_Lift x)
        SureQuery (Query_Ask f) -> Sure (Any_Ask f)
        SureQuery (Query_Get f) -> Sure (Any_Get f)
        SureQuery (Query_Next f) -> Sure (Any_Next f)
        SureQuery (Query_Join x) -> Sure (Any_Join (cast (fmap cast x)))
        SureQuery (Query_Fail x) -> Sure (Any_Fail \r -> x r)


-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Query where
  cast' :: forall xs x r s e m a. Monad m => SureQuery xs x r s e m a -> Query xs x r s e m a
  cast' (SureQuery q) = mapError absurd q


-- Trivial subtypes of Atom

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance Is Query Atom where
    cast' = Atom . fmap return

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance Is Sure Atom where
    cast' = Atom . return


-- | Fail casts to anything that isn't Sure
instance Is Fail Any where
    cast' (Fail f) = Any_Fail f

-- | Fail casts to anything that isn't Sure
instance Is Fail Query where
    cast' (Fail f) = Query_Fail f

-- | Fail casts to anything that isn't Sure
instance Is Fail Move where
    cast' (Fail f) = Move (Any_Fail f)

-- | Fail casts to anything that isn't Sure
instance Is Fail Atom where
    cast' (Fail f) = Atom (Query_Fail f)

-- | Fail casts to anything that isn't Sure
instance Is Fail AtomicMove where
    cast' (Fail f) = AtomicMove (Atom (Query_Fail f))
