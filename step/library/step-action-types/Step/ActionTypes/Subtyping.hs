{-# language DataKinds, KindSignatures, FunctionalDependencies, InstanceSigs #-}
{-# language DataKinds, InstanceSigs, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

module Step.ActionTypes.Subtyping
  (
    Is (..),
    castTo,
    LossOfMovement,
    AssumeSurity (..),
    AssumeMovement (..),
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Coerce (coerce)

class Is (act1 :: Action) (act2 :: Action) where
    cast :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

castTo :: forall act2 act1 xs x r s e m a. (Monad m, Is act1 act2) =>
    act1 xs x r s e m a -> act2 xs x r s e m a
castTo = cast @act1 @act2

instance Is act (Joining act) where cast = Plain

-- Everything is itself
instance Is Any Any where cast = id
instance Is Base Base where cast = id
instance Is Query Query where cast = id
instance Is Move Move where cast = id
instance Is Atom Atom where cast = id
instance Is AtomicMove AtomicMove where cast = id
instance Is Sure Sure where cast = id
instance Is SureBase SureBase where cast = id
instance Is SureQuery SureQuery where cast = id
instance Is Fail Fail where cast = id

-- SureBase is everything but Move, Fail
instance Is SureBase Any where cast = castTo @Any . castTo @Base
instance Is SureBase Base where cast = mapError absurd . (\(SureBase x) -> x)
instance Is SureBase Query where cast = castTo @Query . castTo @Base
instance Is SureBase Atom where cast = mapError absurd . Atom . cast . fmap trivial . (\(SureBase x) -> x)
instance Is SureBase Sure where cast = Sure . castTo @Any . mapError'
instance Is SureBase SureQuery where cast = SureQuery . Plain

-- Base is everything but Sure Move, Fail
instance Is Base Any where cast = Any . cast . cast @Base @BaseRW
instance Is Base Query where cast = Query . cast
instance Is Base Atom where cast = Atom . cast . fmap trivial
instance Is Base BaseRW where cast = _

-- Everything is Any
instance Is Move Any where cast = coerce
instance Is AtomicMove Any where cast = cast @Atom @Any . coerce
instance Is Sure Any where cast (Sure x) = mapError absurd x
instance Is SureQuery Any where cast = castTo @Any . castTo @Query
instance Is Query Any where cast (Query x) = Any (joiningActionHoist cast x)
instance Is Atom Any where
    cast =
        Any
        . Join
        . fmap (mapError absurd . (\(Any x) -> x) . (\(Sure x) -> x))
        . joiningActionHoist (cast @Base @BaseRW)
        . (\(Query q) -> q)
        . (\(Atom q) -> q)

-- Atom + Move = AtomicMove
instance Is AtomicMove Move where cast = coerce . cast @Atom @Any . coerce
instance Is AtomicMove Atom where cast = coerce

-- Sure + Query = SureQuery
instance Is SureQuery Sure where cast = Sure . Any . joiningActionHoist (cast @Base @BaseRW . (\(SureBase x) -> x)) . (\(SureQuery q) -> q)
instance Is SureQuery Query where cast = Query . joiningActionHoist cast . (\(SureQuery q) -> q)

-- Trivial subtypes of Atom
instance Is Query Atom where cast = Atom . fmap return
instance Is Sure Atom where cast = Atom . return
instance Is SureQuery Atom where cast = castTo @Atom . castTo @Query

-- Fail is anything but Sure
instance Is Fail Base where cast (Fail f) = Base_Fail f
instance Is Fail Any where cast = castTo @Any . castTo @Base
instance Is Fail Query where cast = castTo @Query . castTo @Base
instance Is Fail Move where cast = Move . castTo @Any . castTo @Base
instance Is Fail Atom where cast = castTo @Atom . castTo @Base
instance Is Fail AtomicMove where cast = AtomicMove . cast

class Is act1 act2 => LossOfMovement act1 act2 | act1 -> act2

instance LossOfMovement Any Any
instance LossOfMovement Atom Atom
instance LossOfMovement Sure Sure
instance LossOfMovement Query Query
instance LossOfMovement Move Any
instance LossOfMovement AtomicMove Atom
instance LossOfMovement Fail Fail
instance LossOfMovement SureQuery SureQuery

class Is act2 act1 => AssumeMovement act1 act2 | act1 -> act2 where
    assumeMovement :: act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeMovement Any Move where assumeMovement = Move
instance AssumeMovement Atom AtomicMove where assumeMovement = AtomicMove

class Is act2 act1 => AssumeSurity act1 act2 | act1 -> act2 where
    assumeSurity :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeSurity Any Sure where
    assumeSurity = Sure . mapError (\_ -> error "assumeSurity: assumption failed")

instance AssumeSurity Query SureQuery where
    assumeSurity = SureQuery . joiningActionHoist (SureBase . mapError (\_ -> error "assumeSurity: assumption failed")) . (\(Query q) -> q)
