{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Contravariant where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Coerce

class ContravariantAction (act :: Action) where
    contramapAction :: Monad m => (r -> r) -> act xs x r s m a -> act xs x r s m a

instance ContravariantAction Any where
    contramapAction f = \case
        Any_Ask g -> Any_Ask (g . f)
        x -> x

instance ContravariantAction Query where
    contramapAction f = \case
        Query_Ask g -> Query_Ask (g . f)
        x -> x

instance ContravariantAction Move where
    contramapAction f = coerce . contramapAction @Any f . coerce

instance ContravariantAction Atom where
    contramapAction f (Atom a) = Atom (contramapAction f (fmap (contramapAction f) a))

instance ContravariantAction AtomicMove where
    contramapAction f = coerce . contramapAction @Atom f . coerce

instance ContravariantAction Sure where
    contramapAction f = \case
        Sure_Ask g -> Sure_Ask (g . f)
        x -> x

instance ContravariantAction SureQuery where
    contramapAction f = \case
        SureQuery_Ask g -> SureQuery_Ask (g . f)
        x -> x

instance ContravariantAction Fail where
    contramapAction _ Fail = Fail
