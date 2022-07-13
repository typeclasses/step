{-# language DataKinds, KindSignatures, MultiParamTypeClasses, Trustworthy #-}

module Step.ActionTypes.Subtyping
  (

    cast,

    Is,
    -- The class's method is not exported because there is no reason to define new instances of the class outside of this module.

  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import qualified Step.ActionTypes.Coerce as Coerce

import Coerce (coerce)


class Is (k1 :: ActionKind) (k2 :: ActionKind)
  where
    cast' :: Monad m => k1 error m a -> k2 error m a


cast :: forall k2 k1 error m a. (Monad m, Is k1 k2) => k1 error m a -> k2 error m a
cast = cast' @k1 @k2


-- Functions used for defining instances below

sureToAny :: Functor m => Sure error m a -> Any error m a
sureToAny (Sure p) = Any $ p <&> Right

failureAny :: Monad m => Fail error m a -> Any error m a
failureAny (Fail f) = Any $ return (Left f)


-- Identity

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

-- | Everything trivially lifts to itself
instance Is Fail Fail where cast' = coerce


-- Any supertypes everything else

-- | Everything lifts to Any
instance Is Query Any where cast' = coerce

-- | Everything lifts to Any
instance Is Move Any where cast' = coerce

-- | Everything lifts to Any
instance Is AtomicMove Any where cast' = coerce

-- | Everything lifts to Any
instance Is Sure Any where cast' = sureToAny . Coerce.to

-- | Everything lifts to Any
instance Is SureQuery Any where cast' = sureToAny . Coerce.to @Sure

-- | Everything lifts to Any
instance Is Atom Any where cast' = coerce


-- Atom + Move = AtomicMove

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Move where cast' = coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Atom where cast' = coerce


-- Sure + Query = SureQuery

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Sure where cast' = coerce

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Query where cast' = Coerce.from @Any . sureToAny . Coerce.to @Sure


-- Trivial subtypes of Atom

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance Is Query Atom where cast' = coerce

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance Is Sure Atom where cast' = Coerce.from @Any . sureToAny


-- Fail casts to anything that isn't Sure

instance Is Fail Any where cast' = failureAny
instance Is Fail Query where cast' = Coerce.from @Any . failureAny
instance Is Fail Move where cast' = Coerce.from @Any . failureAny
instance Is Fail Atom where cast' = Coerce.from @Any . failureAny
instance Is Fail AtomicMove where cast' = Coerce.from @Any . failureAny
