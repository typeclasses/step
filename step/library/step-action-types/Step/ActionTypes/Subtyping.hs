{-# language DataKinds, InstanceSigs, KindSignatures, MultiParamTypeClasses #-}

module Step.ActionTypes.Subtyping
  (
    Is (..),
    castTo,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.Fallible

import Coerce (coerce)

class Is (act1 :: Action) (act2 :: Action) where
    cast :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

castTo :: forall act2 act1 xs x r s e m a. (Monad m, Is act1 act2) =>
    act1 xs x r s e m a -> act2 xs x r s e m a
castTo = cast @act1 @act2

-- Everything trivially lifts to itself
instance Is Any Any where cast = id
instance Is Query Query where cast = id
instance Is Move Move where cast = id
instance Is Atom Atom where cast = id
instance Is AtomicMove AtomicMove where cast = id
instance Is Sure Sure where cast = id
instance Is SureQuery SureQuery where cast = id
instance Is Fail Fail where cast = id

-- Everything lifts to Any
instance Is Move Any where cast = coerce
instance Is AtomicMove Any where cast = cast @Atom @Any . coerce
instance Is Sure Any where cast (Sure x) = mapError absurd x
instance Is SureQuery Any where cast (SureQuery x) = castTo @Any (mapError absurd x)
instance Is Atom Any where cast (Atom q) = Any_Join (castTo @Any (fmap (castTo @Any) q))
instance Is Query Any where
    cast = \case
        Query_Lift x -> Any_Lift x
        Query_Ask f -> Any_Ask f
        Query_Get f -> Any_Get f
        Query_Next f -> Any_Next f
        Query_Join x -> Any_Join (castTo @Any $ fmap (castTo @Any) x)
        Query_Fail f -> Any_Fail f
        Query_Reset x -> Any_Reset x

-- Atom + Move = AtomicMove
instance Is AtomicMove Move where cast = coerce . cast @Atom @Any . coerce
instance Is AtomicMove Atom where cast = coerce

-- Sure + Query = SureQuery
instance Is SureQuery Sure where cast (SureQuery q) = Sure (cast q)
instance Is SureQuery Query where cast (SureQuery q) = mapError absurd q

-- Trivial subtypes of Atom
instance Is Query Atom where cast = Atom . fmap return
instance Is Sure Atom where cast = Atom . return
instance Is SureQuery Atom where cast (SureQuery q) = Atom $ fmap return $ mapError absurd q

-- Fail casts to anything that isn't Sure
instance Is Fail Any where cast (Fail f) = Any_Fail f
instance Is Fail Query where cast (Fail f) = Query_Fail f
instance Is Fail Move where cast (Fail f) = Move (Any_Fail f)
instance Is Fail Atom where cast (Fail f) = Atom (Query_Fail f)
instance Is Fail AtomicMove where cast (Fail f) = AtomicMove (Atom (Query_Fail f))
