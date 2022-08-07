{-# language DataKinds, InstanceSigs, KindSignatures, MultiParamTypeClasses, Trustworthy #-}

module Step.ActionTypes.Subtyping
  (

    cast,

    Is,
    -- The class's method is not exported because there is no reason to define new instances of the class outside of this module.

  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Coerce (coerce)

import Step.Cursor (readOnly')


class Is (act1 :: Action) (act2 :: Action) where
    cast' :: Monad m => act1 xs x r s m a -> act2 xs x r s m a


cast :: forall act2 act1 xs x r s m a. (Monad m, Is act1 act2) => act1 xs x r s m a -> act2 xs x r s m a
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
instance Is Query Any where cast' = cast @Any . cast @Atom

-- | Everything lifts to Any
instance Is Move Any where cast' = coerce

-- | Everything lifts to Any
instance Is AtomicMove Any where cast' = cast' @Atom @Any . coerce

-- | Everything lifts to Any
instance Is Sure Any where
  cast' :: forall xs x r s m a. Monad m => Sure xs x r s m a -> Any xs x r s m a
  cast' = r
    where
      r :: forall a'. Sure xs x r s m a' -> Any xs x r s m a'
      r = \case
        Sure_Lift x -> Any_Lift x
        Sure_Ask f -> Any_Ask f
        Sure_Get f -> Any_Get f
        Sure_Next f -> Any_Next f
        Sure_Commit n f -> Any_Commit n f
        Sure_Join x -> Any_Join (r (fmap r x))

-- | Everything lifts to Any
instance Is SureQuery Any where cast' = cast @Any . cast @Query

-- | Everything lifts to Any
instance Is Atom Any where
  cast' (Atom q) = Any_Join (cast @Any (fmap (cast @Any) q))


-- Atom + Move = AtomicMove

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Move where
  cast' = coerce . cast' @Atom @Any . coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Atom where cast' = coerce


-- Sure + Query = SureQuery

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Sure where
  cast' :: forall xs x r s m a. Monad m => SureQuery xs x r s m a -> Sure xs x r s m a
  cast' = r
    where
      r :: forall a'. SureQuery xs x r s m a' -> Sure xs x r s m a'
      r = \case
        SureQuery_Lift x -> Sure_Lift x
        SureQuery_Ask f -> Sure_Ask f
        SureQuery_Get f -> Sure_Get f
        SureQuery_Next f -> Sure_Next f
        SureQuery_Join x -> Sure_Join (r (fmap r x))


-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Query where
  cast' :: forall xs x r s m a. Monad m => SureQuery xs x r s m a -> Query xs x r s m a
  cast' = r
    where
      r :: forall a'. SureQuery xs x r s m a' -> Query xs x r s m a'
      r = \case
        SureQuery_Lift x -> Query_Lift x
        SureQuery_Ask f -> Query_Ask f
        SureQuery_Get f -> Query_Get f
        SureQuery_Next f -> Query_Next f
        SureQuery_Join x -> Query_Join (r (fmap r x))


-- Trivial subtypes of Atom

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance Is Query Atom where
  cast' = Atom . fmap return

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance Is Sure Atom where cast' = Atom . return


-- | Fail casts to anything that isn't Sure
instance Is Fail Any where cast' Fail = Any_Fail id

-- | Fail casts to anything that isn't Sure
instance Is Fail Query where cast' Fail = Query_Fail id

-- | Fail casts to anything that isn't Sure
instance Is Fail Move where cast' Fail = Move (Any_Fail id)

-- | Fail casts to anything that isn't Sure
instance Is Fail Atom where cast' Fail = Atom (Query_Fail id)

-- | Fail casts to anything that isn't Sure
instance Is Fail AtomicMove where cast' Fail = AtomicMove (Atom (Query_Fail id))
