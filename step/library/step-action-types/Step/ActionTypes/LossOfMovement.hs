{-# language DataKinds, KindSignatures, FunctionalDependencies, Trustworthy #-}

module Step.ActionTypes.LossOfMovement where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.Subtyping

class Is act1 act2 =>
    LossOfMovement (act1 :: Action) (act2 :: Action)
    | act1 -> act2

instance LossOfMovement Any Any
instance LossOfMovement Atom Atom
instance LossOfMovement Sure Sure
instance LossOfMovement Query Query
instance LossOfMovement Move Any
instance LossOfMovement AtomicMove Atom
instance LossOfMovement Fail Fail
instance LossOfMovement SureQuery SureQuery
