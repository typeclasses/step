{-# language DataKinds, KindSignatures #-}

module Step.ActionTypes.Contravariant where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Coerce

class ContravariantAction (act :: Action) where
    contramapAction :: Monad m => (r -> r) -> act xs x r s e m a -> act xs x r s e m a

instance ContravariantAction Base where
    contramapAction f = \case
        Base_Ask g -> Base_Ask (g . f)
        Base_Fail g -> Base_Fail (g . f)
        x -> x

instance ContravariantAction Any where
    contramapAction f = \case
        Any_Base x -> Any_Base (contramapAction f x)
        Any_Join x -> Any_Join (contramapAction f (fmap (contramapAction f) x))
        x -> x

instance ContravariantAction Query where
    contramapAction f = \case
        Query_Base x -> Query_Base (contramapAction f x)
        Query_Join x -> Query_Join (contramapAction f (fmap (contramapAction f) x))

instance ContravariantAction Move where
    contramapAction f = coerce . contramapAction @Any f . coerce

instance ContravariantAction Atom where
    contramapAction f (Atom a) = Atom (contramapAction f (fmap (contramapAction f) a))

instance ContravariantAction AtomicMove where
    contramapAction f = coerce . contramapAction @Atom f . coerce

instance ContravariantAction Sure where
    contramapAction f (Sure x) = Sure (contramapAction f x)

instance ContravariantAction SureQuery where
    contramapAction f (SureQuery x) = SureQuery (contramapAction f x)

instance ContravariantAction Fail where
    contramapAction f (Fail g) = Fail (g . f)
