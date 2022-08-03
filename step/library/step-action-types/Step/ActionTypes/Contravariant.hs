{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Contravariant where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class ContravariantAction (act :: Action) where
    contramapAction :: Monad m => (r -> r) -> act xs x r s m a -> act xs x r s m a

instance ContravariantAction Any where
    contramapAction f (Any a) =
        Any \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction Query where
    contramapAction f (Query a) =
        Query \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction Move where
    contramapAction f (Move a) =
        Move \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction Atom where
    contramapAction f (Atom a) =
        Atom \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction AtomicMove where
    contramapAction f (AtomicMove a) =
        AtomicMove \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction Sure where
    contramapAction f (Sure a) =
        Sure \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction SureQuery where
    contramapAction f (SureQuery a) =
        SureQuery \c -> ask >>= \r ->
            contramap f (a (contraconst r c))

instance ContravariantAction Fail where
    contramapAction f (Fail a) =
        Fail \c -> ask >>= \r ->
            contramap f (a (contraconst r c))
