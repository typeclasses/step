module Step.Action.Config where

import Step.Internal.Prelude

import Step.Action.Constructors

class ConfigurableAction (action :: ActionKind) where
    configureAction :: (config1 -> config2)
        -> action config2 cursor error m a
        -> action config1 cursor error m a

instance ConfigurableAction Any where
    configureAction f (Any g) = Any (g . f)
instance ConfigurableAction Query where
    configureAction f (Query g) = Query (g . f)
instance ConfigurableAction Move where
    configureAction f (Move g) = Move (g . f)
instance ConfigurableAction Atom where
    configureAction f (Atom g) = Atom (g . f)
instance ConfigurableAction AtomicMove where
    configureAction f (AtomicMove g) = AtomicMove (g . f)
instance ConfigurableAction Sure where
    configureAction f (Sure g) = Sure (g . f)
instance ConfigurableAction SureQuery where
    configureAction f (SureQuery g) = SureQuery (g . f)
