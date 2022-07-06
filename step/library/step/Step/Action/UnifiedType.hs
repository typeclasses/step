module Step.Action.UnifiedType where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import Step.Action.Functor (FunctorAction)

import qualified Variado.Monad.Class as V

import qualified Monad

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

newtype Action (k :: ActionKind) config cursor error m a =
    Action (k config cursor error m a)

instance (Functor m, FunctorAction k) => Functor (Action k config cursor error m) where
    fmap f = Action . fmap f . (\(Action a) -> a)

instance (Monad m, FunctorAction k, T.MonadAction k) => Applicative (Action k config cursor error m) where
    pure = Action . T.pureAction
    (<*>) = Monad.ap

instance (Monad m, FunctorAction k, T.MonadAction k) => Monad (Action k config cursor error m) where
    a >>= b = Action (T.bindAction ((\(Action x) -> x) a) (fmap ((\(Action x) -> x)) b))

---

class AlwaysMoves (k :: ActionKind)
instance AlwaysMoves T.Move
instance AlwaysMoves T.MoveUndo
instance AlwaysMoves T.SureMove

---

class Noncommittal (k :: ActionKind)
  where
    type Try k :: ActionKind
    try :: Functor m => Action k config cursor error m a -> Action (Try k) config cursor error m (Maybe a)

instance Noncommittal T.Undo
  where
    type Try T.Undo = T.Sure
    try = Action . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . (\(Action a) -> a)

instance Noncommittal T.MoveUndo
  where
    type Try T.MoveUndo = T.SureMove
    try = Action . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . (\(Action a) -> a)

instance Noncommittal T.Static
  where
    type Try T.Static = T.SureStatic
    try = Action . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . (\(Action a) -> a)

---

class CanFail (k :: ActionKind)
  where
    failure :: Monad m => (config -> StateT cursor m error) -> Action k config cursor error m a

instance CanFail T.Any      where failure = Action . view Coerce.coerced . T.failureAny
instance CanFail T.Static   where failure = Action . view Coerce.coerced . T.failureAny
instance CanFail T.Move     where failure = Action . view Coerce.coerced . T.failureAny
instance CanFail T.Undo     where failure = Action . view Coerce.coerced . T.failureAny
instance CanFail T.MoveUndo where failure = Action . view Coerce.coerced . T.failureAny

---

type IsAction k =
    ( SureStaticId k
    , FunctorAction k
    )

---

class
    ( ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
    => SureStaticId k

instance SureStaticId T.Any
instance SureStaticId T.Static
instance SureStaticId T.Move
instance SureStaticId T.Undo
instance SureStaticId T.MoveUndo
instance SureStaticId T.Sure
instance SureStaticId T.SureStatic
instance SureStaticId T.SureMove

---

configure :: (IsAction k, T.ConfigurableAction k) =>
    (config1 -> config2)
    -> Action k config2 cursor error m a
    -> Action k config1 cursor error m a
configure f = Action . T.configureAction f . (\(Action a) -> a)

---

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => Action k1 config cursor error m (Action k2 config cursor error m a) -> Action (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Sure       T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.Move       T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.MoveUndo   T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Sure       T.SureMove   where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureMove   T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.MoveUndo   T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.MoveUndo   T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Undo       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Undo       T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Static     where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Static     T.Move       where actionJoin = cj CJ.anyToAny

instance ActionJoin T.Any        T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Any        where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Static     T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Static     where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Move       T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Move       where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Undo       T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Undo       where actionJoin = cj CJ.sureToAny
instance ActionJoin T.MoveUndo   T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.MoveUndo   where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Sure       T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureMove   T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.SureMove   where actionJoin = cj CJ.sureToSure

cj ::
    (Functor (k1 config cursor error m)) =>
    (
      k1 config cursor error m (k2 config cursor error m a)
      -> (k1 :> k2) config cursor error m a
    )
    -> Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
cj f = Action . f . fmap ((\(Action a) -> a)) . (\(Action a) -> a)

---

instance (ActionJoin k1 k2, Monad m) => V.PolyMonad (Action k1 config cursor error m) (Action k2 config cursor error m)
  where
    type Join (Action k1 config cursor error m) (Action k2 config cursor error m) = Action (k1 :> k2) config cursor error m
    join = actionJoin
