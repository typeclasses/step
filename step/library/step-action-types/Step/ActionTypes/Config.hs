{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Config where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Configurable (action :: ActionKind) where
    configure :: (config1 -> config2)
        -> action config2 cursor error m a
        -> action config1 cursor error m a

instance Configurable Any where
    configure f (Any g) = Any (g . f)
instance Configurable Query where
    configure f (Query g) = Query (g . f)
instance Configurable Move where
    configure f (Move g) = Move (g . f)
instance Configurable Atom where
    configure f (Atom g) = Atom (g . f)
instance Configurable AtomicMove where
    configure f (AtomicMove g) = AtomicMove (g . f)
instance Configurable Sure where
    configure f (Sure g) = Sure (g . f)
instance Configurable SureQuery where
    configure f (SureQuery g) = SureQuery (g . f)
