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

import Step.Cursor (cursorR)


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
instance Is AtomicMove Any where cast' = coerce

-- | Everything lifts to Any
instance Is Sure Any where cast' (Sure p) = Any \c -> p c <&> Right

-- | Everything lifts to Any
instance Is SureQuery Any where cast' = cast @Any . cast @Query

-- | Everything lifts to Any
instance Is Atom Any where cast' = coerce


-- Atom + Move = AtomicMove

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Move where cast' = coerce

-- | AtomicMove gets its name from the fact that it has the properties of both Atom and Move
instance Is AtomicMove Atom where cast' = coerce


-- Sure + Query = SureQuery

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Sure where cast' (SureQuery p) = Sure \c -> p (cursorR c)

-- | SureQuery gets its name from the fact that it has the properties of both Sure and Query
instance Is SureQuery Query where cast' (SureQuery p) = Query \c -> p c <&> Right


-- Trivial subtypes of Atom

-- | A Query is trivially atomic because it never moves the cursor, therefore it cannot move and fail
instance Is Query Atom where cast' (Query p) = Atom \c -> p (cursorR c)

-- | A Sure action is trivially atomic because it never fails, therefore it cannot move and fail
instance Is Sure Atom where cast' = Coerce.from @Any . cast @Any


-- | Fail casts to anything that isn't Sure
instance Is Fail Any where cast' Fail = Any \_ -> ask <&> Left

-- | Fail casts to anything that isn't Sure
instance Is Fail Query where cast' Fail = Query \_ -> ask <&> Left

-- | Fail casts to anything that isn't Sure
instance Is Fail Move where cast' = Coerce.from @Any . cast @Any

-- | Fail casts to anything that isn't Sure
instance Is Fail Atom where cast' = Coerce.from @Any . cast @Any

-- | Fail casts to anything that isn't Sure
instance Is Fail AtomicMove where cast' = Coerce.from @Any . cast @Any
