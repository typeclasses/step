module Step.Action.SeparateTypes where

import Step.Internal.Prelude

import Step.Action.Kinds

---

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case
    Left _ -> Nothing
    Right x -> Just x

---

class ConfigurableAction (action :: ActionKind) where
    configureAction :: (config1 -> config2)
        -> action config2 cursor error m a
        -> action config1 cursor error m a

instance ConfigurableAction Any where
    configureAction f (Any g) = Any (g . f)
instance ConfigurableAction Static where
    configureAction f (Static g) = Static (g . f)
instance ConfigurableAction Move where
    configureAction f (Move g) = Move (g . f)
instance ConfigurableAction Undo where
    configureAction f (Undo g) = Undo (g . f)
instance ConfigurableAction MoveUndo where
    configureAction f (MoveUndo g) = MoveUndo (g . f)
instance ConfigurableAction Sure where
    configureAction f (Sure g) = Sure (g . f)
instance ConfigurableAction SureStatic where
    configureAction f (SureStatic g) = SureStatic (g . f)
instance ConfigurableAction SureMove where
    configureAction f (SureMove g) = SureMove (g . f)
