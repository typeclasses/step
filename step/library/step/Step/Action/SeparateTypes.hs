module Step.Action.SeparateTypes where

import Step.Internal.Prelude

import qualified Monad

import Step.Action.Kinds

---

sureToAny :: Functor m => Sure config cursor error m a -> Any config cursor error m a
sureToAny (Sure p) = Any (\c s -> p c s <&> \(x, s') -> (Right x, s'))

---

joinAnyToSure :: Monad m => Any config cursor error m (Sure config cursor error m a) -> Any config cursor error m a
joinAnyToSure (Any p) =
    Any \c s -> do
        (e, s') <- p c s
        case e of
            Left e' -> return (Left e', s')
            Right (Sure p') -> do
                (x, s'') <- p' c s'
                return (Right x, s'')

joinSureToAny :: Monad m => Sure config cursor error m (Any config cursor error m a) -> Any config cursor error m a
joinSureToAny (Sure p) =
    Any \c s -> do
        (Any p', s') <- p c s
        p' c s'

---

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c s -> p c s <&> \(e, s') -> case e of
    Left _ -> (Nothing, s')
    Right x -> (Just x, s')

---

failureAny :: Monad m => (config -> (StateT cursor m error)) -> Any config cursor error m a
failureAny f = Any \c s -> return (Left (f c), s)

---

class Like k k'
  where
    actionIso :: Iso
        (k' config1 cursor1 error1 m1 a1)
        (k' config2 cursor2 error2 m2 a2)
        (k  config1 cursor1 error1 m1 a1)
        (k  config2 cursor2 error2 m2 a2)

actionIso' :: forall k k' config1 cursor1 error1 m1 a1 config2 cursor2 error2 m2 a2.
    Like k k' => Iso
        (k' config1 cursor1 error1 m1 a1)
        (k' config2 cursor2 error2 m2 a2)
        (k  config1 cursor1 error1 m1 a1)
        (k  config2 cursor2 error2 m2 a2)
actionIso' = actionIso

instance Like Any Any         where actionIso = coerced
instance Like Any Static      where actionIso = coerced
instance Like Any Move        where actionIso = coerced
instance Like Any Undo        where actionIso = coerced
instance Like Any MoveUndo    where actionIso = coerced

instance Like Sure Sure       where actionIso = coerced
instance Like Sure SureStatic where actionIso = coerced
instance Like Sure SureMove   where actionIso = coerced

---

class FunctorAction (action :: ActionKind) where
    fmapAction :: Functor m => (a -> b)
        -> action config cursor error m a
        -> action config cursor error m b

instance FunctorAction Any        where fmapAction = fmap
instance FunctorAction Static     where fmapAction = fmap
instance FunctorAction Move       where fmapAction = fmap
instance FunctorAction Undo       where fmapAction = fmap
instance FunctorAction MoveUndo   where fmapAction = fmap
instance FunctorAction Sure       where fmapAction = fmap
instance FunctorAction SureStatic where fmapAction = fmap
instance FunctorAction SureMove   where fmapAction = fmap

---

class MonadAction (action :: ActionKind) where
    pureAction :: Monad m => a -> action config cursor error m a
    bindAction :: Monad m => action config cursor error m a -> (a -> action config cursor error m b) -> action config cursor error m b

instance MonadAction Any        where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Static     where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Move       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Sure       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureStatic where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureMove   where pureAction = pure ; bindAction = (Monad.>>=)

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
